library(shinyBS)
source("processingtasks_module.R")
source("functions.R")

supported_formats <- c(
  "MassBank" = "MsFormatMassbank", 
  "MSP" = "MsFormatMsp", 
  "MGF" = "MsFormatMgf")

settings_module_ui <- function(id) {
  tagList(
    bsCollapse(
      bsCollapsePanel(
        "Input & output", 
        fluidRow(
          column(6, 
                 strong("Input directory"),
                 shinyDirButton(NS(id, "inputdir"), 
                                label = "Select input dir",
                                title = "Select input dir"),
                 textOutput(NS(id, "inputdir")),
                 selectInput(NS(id, "inputformat"), label = "Input format", 
                             choices = supported_formats),
                 strong("Input mapping"),
                 textOutput(NS(id, "inputmap")), 
                 actionLink(NS(id, "inputmap_edit"), "edit"),
                 checkboxInput(NS(id, "input_unzip"), "Unzip input")
                 ),
          column(6,
                 strong("Output directory"),
                 shinyDirButton(NS(id, "outputdir"),
                                label = "Select output dir",
                                title = "Select output dir"),
                 textOutput(NS(id, "outputdir")),
                 selectInput(NS(id, "outputformat"), label = "Output format", 
                             choices = supported_formats),
                 strong("Output mapping"),
                 textOutput(NS(id, "outputmap")),
                 actionLink(NS(id, "outputmap_edit"), "edit"),
                 checkboxInput(NS(id, "output_wipe"), "Wipe old output"),
                 checkboxInput(NS(id, "output_zip"), "Zip output")
                 )
          ),
        textInput(NS(id, "filename_out_schema"), "Output filename {schema}")
      ),
      bsCollapsePanel("Extra processing", 
      # processingtasks_module_ui(NS(id, "processingtasks")),
        actionButton(NS(id, "processingtasks"), label = "Extra processing tasks")
      ),
      bsCollapsePanel("Chunking",
        numericInput(NS(id, "spectra_per_file"), "Spectra per file", "-1"),
        numericInput(NS(id, "files_per_block"), "Files per block", "1"),
        strong("Cache directory"),
        shinyDirButton(NS(id, "cachedir"),
                       label = "Select cache dir",
                       title = "Select cache dir"),
        textOutput(NS(id, "cachedir"))
      )
    ), # bsCollapse
    strong("Raw settings"), br(),
    editor_module_ui(NS(id, "settings_yaml"))
    )
}

settings_module_server <- function(id, 
                                   inputmap,  outputmap, 
                                   tab_input = "inputmap",
                                   tab_output = "outputmap") {
  
  stopifnot(is.reactive(inputmap))
  stopifnot(is.reactive(outputmap))
  stopifnot(!is.reactive(tab_input))
  stopifnot(!is.reactive(tab_output))

  moduleServer(id, function(input, output, session) {
    
    shinyDirChoose(input, "inputdir", roots=c(input="."), allowDirCreate = FALSE)
    shinyDirChoose(input, "outputdir", roots=c(input="."), allowDirCreate = TRUE)
    shinyDirChoose(input, "cachedir", roots=c(input="."), allowDirCreate = TRUE)
    
    inputdir_ <- reactive({parseDirPath(roots=c(input="."), input$inputdir)})
    inputdir <- reactiveVal()
    observeEvent(inputdir_(), {inputdir(inputdir_())})
    
    outputdir_ <- reactive({parseDirPath(roots=c(input="."), input$outputdir)})
    outputdir <- reactiveVal()
    observeEvent(outputdir_(), {outputdir(outputdir_())})
    
    cachedir_ <- reactive({parseDirPath(roots=c(input="."), input$cachedir)})
    cachedir <- reactiveVal()
    observeEvent(cachedir_(), {cachedir(cachedir_())})
    
    output$inputdir <- renderText({inputdir()})
    output$outputdir <- renderText({outputdir()})
    output$cachedir <- renderText({cachedir()})
    output$inputmap <- renderText({inputmap()})
    output$outputmap <- renderText({outputmap()})
    
    
    processingtasks <- reactiveVal(list())
    
    
    
    inputContent <- reactiveVal("")
    editor <- editor_module_server(
      "settings_yaml", 
      inputContent = inputContent,
      returnContent = TRUE)

    settings_list <- reactive({
      tryCatch(list(
        format = list(
          input = input$inputformat,
          output = input$outputformat
        ),
        mapping = list(
          input = inputmap(),
          output = outputmap()
        ),
        data = list(
          input = inputdir(),
          output = outputdir(),
          cache = cachedir(),
          input_unzip = input$input_unzip,
          output_zip = input$output_zip,
          output_wipe = input$output_wipe
        ),
        processing = processingtasks(),
        spectra_per_file = input$spectra_per_file,
        files_per_block = input$files_per_block,
        filename_out_schema = input$filename_out_schema
      ),
      error = function(e) list())
    })
    
    # Update editor view from current settings
    observeEvent(settings_list(), {
        inputContent(yaml::as.yaml(settings_list()))
    })
    
    tabSelect <- reactiveVal(NULL)
    observeEvent(input$inputmap_edit, {
      message(tab_input)
      tabSelect("")
      tabSelect(tab_input)
    })
    observeEvent(input$outputmap_edit, {
      message(tab_output)
      tabSelect("")
      tabSelect(tab_output)
    })
    
    # Respond to file loading in editor
    observeEvent(editor(), {
      settings_data <- yaml::yaml.load(editor())
      # set formats
      updateSelectInput(session, "inputformat", selected = settings_data$format$input)
      updateSelectInput(session, "outputformat", selected = settings_data$format$output)
      # set data dirs
      inputdir(settings_data$data$input)
      outputdir(settings_data$data$output)
      cachedir(settings_data$data$cache)
      
      updateCheckboxInput(session, "input_unzip", value = settings_data$data$input_unzip)
      updateCheckboxInput(session, "output_zip", value = settings_data$data$output_zip)
      updateCheckboxInput(session, "output_wipe", value = settings_data$data$output_wipe)
      
      # set chunking behaviour
      updateNumericInput(session, "spectra_per_file", value = settings_data$spectra_per_file)
      updateNumericInput(session, "files_per_block", value = settings_data$files_per_block)
      # set mapping files, these are "shared" reactiveVal that will also trigger 
      # the file load in the target editor
      inputmap(settings_data$mapping$input)
      outputmap(settings_data$mapping$output)
      
      processingtasks(settings_data$processing)
      # set filename
      updateTextInput(session, "filename_out_schema", value = settings_data$filename_out_schema)
    })
    
    processingtasks_edit <- modalDialog(processingtasks_module_ui(NS(id, "processingtasks_edit"), process_spectra))
    processingtasks_module_server("processingtasks_edit", processingtasks, process_spectra)
    observeEvent(input$processingtasks, {
      showModal(processingtasks_edit)
      processingtasks_  <- processingtasks()
      processingtasks(NULL)
      processingtasks(processingtasks_)
    })
    
    return(list(
      settings = reactive({ settings_list() }),
      tabSelect = reactive({ tabSelect() })
      ))
    
  }) # moduleServer
} # settings_module_server

