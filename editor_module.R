

editor_module_ui <- function(id) {
  tagList(
    shinyFilesButton(NS(id, "choose"), "Select/Open",
                     "Choose File", 
                     title="Choose File", 
                     multiple=FALSE, buttonType="default", 
                     startRoot=c(input=".")),
    actionButton(NS(id, "save"), "Save"),
    shinySaveButton(NS(id, "saveas"), "Save As", 
                    title="Save As",
                    multiple=FALSE, 
                    buttonType="default", 
                    startRoot=c(input=".")),
    textOutput(NS(id, "filename")),
    aceEditor(NS(id, "editor"), mode = "yaml")
    )
}

editor_module_server <- function(id, inputContent = reactive({""})) {
  
  stopifnot(is.reactive(inputContent))
  
  moduleServer(id, function(input, output, session) {
    
    fileName <- reactiveVal(NULL)
    # activate shinyFile tools
    shinyFileChoose(input, "choose", roots=c(input="."), session=session)
    shinyFileSave(input, "saveas", roots=c(input="."), session=session)
    # set filename on file selection
    observeEvent(input$choose, {
      fileName(
        parseFilePaths(
          roots = c(input="."), 
          input$choose)$datapath[1]
      )
      message(fileName())
    })
    # show filename in ui
    output$filename <- renderText({ fileName() })
    
    # load content when file is chosen
    observeEvent(fileName(), {
      req(fileName())  # Ensure filename exists
      content <- paste(readLines(fileName(), warn = FALSE), collapse = "\n")
      updateAceEditor(session, "editor", value = content)
    })
    
    # overwrite file on "Save"
    observeEvent(input$save, {
      req(fileName())  # Ensure filename exists
      content <- input$editor
      write_lines(content, fileName())
    })
    
    # on "Save as", write to new file and change file name
    observeEvent(input$saveas, {
      if(is.integer(input$saveas))
        return()
      fileName_ <- parseSavePath(
        roots = c(input="."), 
        input$saveas)$datapath[1]
      content <- input$editor
      write_lines(content, fileName_)
      fileName(fileName_)
    })
    
    inputContent_ <- reactive({ inputContent() })
    
    # Update editor when content arrives
    observeEvent(inputContent_(), {
      data <- inputContent_()
      updateAceEditor(session, "editor", value = data)
    })
    
    return(reactive({fileName()}))
    
  })
}