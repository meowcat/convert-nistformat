

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
    textOutput("filename"),
    aceEditor(NS(id, "editor"), mode = "yaml")
    )
}

editor_module_server <- function(id) {
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
    })
    # show filename in ui
    output$filename <- filename()
    
    # load content when file is chosen
    observeEvent(fileName(), {
      message("save")
      req(fileName())  # Ensure filename exists
      message(fileName())
      content <- paste(readLines(fileName(), warn = FALSE), collapse = "\n")
      updateAceEditor(session, "editor", value = content)
    })
    
    
  })
}