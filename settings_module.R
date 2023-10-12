
supported_formats <- c("MsFormatMassbank", "MsFormatMsp")

settings_module_ui <- function(id) {
  tagList(
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
             actionLink(NS(id, "inputmap_edit"), "edit")
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
             actionLink(NS(id, "outputmap_edit"), "edit")
             )
    ),
    hr(),
    numericInput(NS(id, "spectra_per_file"), "Spectra per file", "-1"),
    numericInput(NS(id, "files_per_block"), "Files per block", "1"),
    
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
    
    inputdir <- reactive({parseDirPath(roots=c(input="."), input$inputdir)})
    outputdir <- reactive({parseDirPath(roots=c(input="."), input$outputdir)})
    
    output$inputdir <- renderText({inputdir()})
    output$outputdir <- renderText({outputdir()})
    output$inputmap <- renderText({inputmap()})
    output$outputmap <- renderText({outputmap()})
    
    inputContent <- reactiveVal("")
    editor <- editor_module_server("settings_yaml", inputContent = inputContent)

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
          output = outputdir()
        ),
        spectra_per_file = input$spectra_per_file,
        files_per_block = input$files_per_block
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
    
    
    return(list(
      settings = reactive({ settings_list() }),
      tabSelect = reactive({ tabSelect() })
      ))
    
  }) # moduleServer
} # settings_module_server

