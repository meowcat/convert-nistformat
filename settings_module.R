
supported_formats <- c("MsFormatMassbank", "MsFormatMsp")

settings_module_ui <- function(id) {
  tagList(
    
    shinyDirButton(NS(id, "inputdir"), 
                   label = "Select input dir",
                   title = "Select input dir"),
    textOutput(NS(id, "inputdir")),
    selectInput(NS(id, "inputformat"), label = "Input format", 
                choices = supported_formats),
    textOutput(NS(id, "inputmap")),
    shinyDirButton(NS(id, "outputdir"),
                   label = "Select output dir",
                   title = "Select output dir"),
    textOutput(NS(id, "outputdir")),
    selectInput(NS(id, "outputformat"), label = "Output format", 
                choices = supported_formats),
    textOutput(NS(id, "outputmap")),
    numericInput(NS(id, "spectra_per_file"), "Spectra per file", "-1"),
    numericInput(NS(id, "files_per_block"), "Files per block", "1"),
    
    aceEditor(NS(id, "editor"), mode = "yaml")
    )
}

settings_module_server <- function(id, inputmap,  outputmap) {
  
  stopifnot(is.reactive(inputmap))
  stopifnot(is.reactive(outputmap))
  
  moduleServer(id, function(input, output, session) {
    
    shinyDirChoose(input, "inputdir", roots=c(input="."), allowDirCreate = FALSE)
    shinyDirChoose(input, "outputdir", roots=c(input="."), allowDirCreate = TRUE)
    
    inputdir <- reactive({parseDirPath(roots=c(input="."), input$inputdir)})
    outputdir <- reactive({parseDirPath(roots=c(input="."), input$outputdir)})
    
    output$inputdir <- renderText({inputdir()})
    output$outputdir <- renderText({outputdir()})
    output$inputmap <- renderText({inputmap()})
    output$outputmap <- renderText({outputmap()})

    settings_list <- reactive({
      list(
        format = list(
          input = input$inputformat,
          output = output$inputformat
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
        files_per_block = input$files_per_block,
      )
    })
    
    return(reactive({settings_list()}))
    
  })
}

