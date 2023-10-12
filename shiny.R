# Required packages
# install.packages(c("shiny", "shinyAce", "callr", "fs", "shinydashboard", "shinyFiles"))

library(shiny)
library(shinyAce)
library(callr)
library(fs)
library(shinydashboard)
library(shinyFiles)

source("editor_module.R")
source("settings_module.R")

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
  ),
  dashboardBody(
    tabsetPanel(
      tabPanel("Home",
               actionButton("start", "Start"),
               actionButton("kill", "Kill"),
               verbatimTextOutput("log")),
      tabPanel("Settings", settings_module_ui("settings")),
      tabPanel("Input map", editor_module_ui("inputmap")),
      tabPanel("Output map", editor_module_ui("outputmap")),
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

  
  file_inputmap <- editor_module_server("inputmap")
  file_outputmap <- editor_module_server("outputmap")
  settings <- settings_module_server("settings",
                                     inputmap = file_inputmap,
                                     outputmap = file_outputmap)
  
  fileNames <- reactiveValues(settings = "", input = "", output = "")
  
  # Background process holder
  process <- reactiveVal(NULL)
  
  # Store logs from background process
  logs <- reactiveVal("")
  

  observeEvent(input$start, {
    shinyjs::disable("start")
    settings_path <- tempfile(fileext = ".yaml")
    yaml::write_yaml(settings, settings_path)
    
    proc <- callr::r_bg(function(settings_file_) { 
      settings_file <- settings_file_
      source("testfile.R") 
      }, 
      args = list(settings_file_ = settings_path),
      supervise = TRUE)
    process(proc)
  })
  
  # observe({
  #   if (!is.null(process())) {
  #     if (process()$is_alive()) {
  #       logs(paste(logs(), process()$read_all_lines(), collapse = "\n"))
  #     } else {
  #       shinyjs::enable("start")
  #       process(NULL)
  #     }
  #   }
  # })
  # 
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
