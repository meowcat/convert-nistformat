# Required packages
# install.packages(c("shiny", "shinyAce", "callr", "fs", "shinydashboard", "shinyFiles"))

library(shiny)
library(shinyAce)
library(callr)
library(fs)
library(shinydashboard)
library(shinyFiles)

modalUI <- function() {
  modalDialog(
    textOutput("current_filename"),
    shinyFilesButton("save_file_browser", "Choose new filename", title="Save As", multiple=FALSE, buttonType="default", startRoot=c(input=".")),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirm_save", "Save")
    )
  )
}

ui <- dashboardPage(
  dashboardHeader(title = "Shiny YAML Editor"),
  dashboardSidebar(
    shinyFilesButton("choose_settings", "Choose Settings", 
                     title="Choose Settings File", 
                     multiple=FALSE, buttonType="default", 
                     startRoot=c(input=".")),
    shinyFilesButton("choose_input", "Choose Input map", 
                     title="Choose Input Map File", 
                     multiple=FALSE, buttonType="default", 
                     startRoot=c(input=".")),
    shinyFilesButton("choose_output", "Choose Output map", 
                     title="Choose Output Map File", 
                     multiple=FALSE, buttonType="default", 
                     startRoot=c(input="."))
  ),
  dashboardBody(
    tabsetPanel(
      tabPanel("Home",
               actionButton("start", "Start"),
               actionButton("kill", "Kill"),
               verbatimTextOutput("log")),
      tabPanel("Settings", 
               actionButton("save_settings", "Save"),
               shinySaveButton("saveas_settings", "Save As", 
                               title="Save As",
                               multiple=FALSE, 
                               buttonType="default", 
                               startRoot=c(input=".")),
               aceEditor("editor1", mode = "yaml")),
      tabPanel("Input map", 
               actionButton("save_input", "Save"), 
               shinySaveButton("saveas_input", "Save As", 
                               title="Save As",
                               multiple=FALSE, 
                               buttonType="default", 
                               startRoot=c(input=".")),
               aceEditor("editor2", mode = "yaml")),
      tabPanel("Output map", 
               actionButton("save_output", "Save"), 
               shinySaveButton("saveas_output", "Save As", 
                               title="Save As",
                               multiple=FALSE, 
                               buttonType="default", 
                               startRoot=c(input=".")),
               aceEditor("editor3", mode = "yaml"))
    )
  )
)

server <- function(input, output, session) {
  
  # Define filenames as a reactiveValues object
  # fileNames <- reactiveValues(
  #   settings = "input/settings.yaml", 
  #   input = "mapping/massbank.yaml", 
  #   output = "mapping/nist-msp.yaml")
  #
  shinyFileChoose(input, 'choose_settings', roots=c(input="."), session=session)
  shinyFileChoose(input, 'choose_input', roots=c(input="."), session=session)
  shinyFileChoose(input, 'choose_output', roots=c(input="."), session=session)
  shinyFileSave(input, "saveas_settings", roots=c(input="."), session=session)
  shinyFileSave(input, "saveas_input", roots=c(input="."), session=session)
  shinyFileSave(input, "saveas_output", roots=c(input="."), session=session)
  
  fileNames <- reactiveValues(settings = "", input = "", output = "")
  
  # Background process holder
  process <- reactiveVal(NULL)
  
  # Store logs from background process
  logs <- reactiveVal("")
  
  # Observe file browser's selected file and set as appropriate type based on clicked button
  
  observeEvent(input$choose_settings, {
     fileNames$settings <- parseFilePaths(roots = c(input="."), input$choose_settings)$datapath[1]
   })
  observeEvent(input$choose_input, {
     fileNames$input <- parseFilePaths(roots = c(input="."), input$choose_input)$datapath[1]
   })
  observeEvent(input$choose_output, {
     fileNames$output <- parseFilePaths(roots = c(input="."), input$choose_output)$datapath[1]
   })
  
  # In the server
  
  # For the Settings editor:
  observeEvent(fileNames$settings, {
    req(fileNames$settings)  # Ensure filename exists
    content <- paste(readLines(fileNames$settings, warn = FALSE), collapse = "\n")
    updateAceEditor(session, "editor1", value = content)
  })
  
  # For the Input map editor:
  observeEvent(fileNames$input, {
    req(fileNames$input)  # Ensure filename exists
    content <- paste(readLines(fileNames$input, warn = FALSE), collapse = "\n")
    updateAceEditor(session, "editor2", value = content)
  })
  
  # For the Output map editor:
  observeEvent(fileNames$output, {
    req(fileNames$output)  # Ensure filename exists
    content <- paste(readLines(fileNames$output, warn = FALSE), collapse = "\n")
    updateAceEditor(session, "editor3", value = content)
  })
  
  
  observeEvent(input$save_settings, {
    write_lines(input$editor1, fileNames$settings)
  })
  observeEvent(input$saveas_settings, {
    if(is.integer(input$saveas_settings))
      return()
    newFile <- parseFilePaths(roots = c(input="."), input$saveas_settings)$datapath[1]
    fileNames$settings <- newFile
    write_lines(input$editor1, fileNames$settings)
  })
  
  observeEvent(input$save_input, {
    write_lines(input$editor2, fileNames$input)
  })
  observeEvent(input$saveas_input, {
    if(is.integer(input$saveas_input))
      return()
    newFile <- parseFilePaths(roots = c(input="."), input$saveas_input)$datapath[1]
    fileNames$settings <- newFile
    write_lines(input$editor2, fileNames$input)
  })
  
  observeEvent(input$save_output, {
    write_lines(input$editor3, fileNames$output)
  })
  observeEvent(input$saveas_output, {
    if(is.integer(input$saveas_output))
      return()
    newFile <- parseFilePaths(roots = c(input="."), input$saveas_output)$datapath[1]
    fileNames$output <- newFile
    write_lines(input$editor3, fileNames$output)
  })
  
  # Saving logic with file overwrite confirmation
  save_logic <- function(editorContent, fileName, fileSlot) {

      output$current_filename <- renderText(fileNames[[fileSlot]])
      # Show modal to choose a new file
      showModal(modalUI())
      
      # Observe confirmation
      observeEvent(input$confirm_save, {
        #writeLines(editorContent, newFile)
        #output$current_filename <- renderText(newFile)
        fileNames[[fileSlot]] <- newFile
      })
  }
  
  observeEvent(input$save1, {
    save_logic(input$editor1, fileNames$settings, "settings")
  })
  
  observeEvent(input$save2, {
    save_logic(input$editor2, fileNames$input, "input")
  })
  
  observeEvent(input$save3, {
    save_logic(input$editor3, fileNames$output, "output")
  })
  
  observeEvent(input$start, {
    shinyjs::disable("start")
    proc <- callr::r_bg(function() { source("process.R") }, supervise = TRUE)
    process(proc)
  })
  
  observe({
    if (!is.null(process())) {
      if (process()$is_alive()) {
        logs(paste(logs(), process()$read_all_lines(), collapse = "\n"))
      } else {
        shinyjs::enable("start")
        process(NULL)
      }
    }
  })
  
  observeEvent(input$kill, {
    if (!is.null(process())) {
      process()$kill()
      process(NULL)
    }
  })
  
  output$log <- renderText({
    logs()
  })
  
}

shinyApp(ui, server)
