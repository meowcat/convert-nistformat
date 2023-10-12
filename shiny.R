# Required packages
# install.packages(c("shiny", "shinyAce", "callr", "fs", "shinydashboard", "shinyFiles"))

library(shiny)
library(shinyAce)
library(callr)
library(fs)
library(shinydashboard)
library(shinyFiles)
library(glue)
library(fansi)

ansi2html <- function(ansi, height = "500px"){
  HTML(sprintf(
    glue("<pre style='height: {height}; overflow: auto'>%s</pre>"),
    gsub("\n", "<br/>", as.character(sgr_to_html(ansi)))
  ))
}

fixed_height <- function(id, height) {
  shiny::tags$head(shiny::tags$style(shiny::HTML(
    glue("#{id} {{ height: {height}; overflow: auto; }}")
  )))  
}


source("editor_module.R")
source("settings_module.R")


ui <- dashboardPage(
  dashboardHeader(title = "SpectraMapping UI"),
  dashboardSidebar(
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    #fixed_height("log", "500px"),
    tabsetPanel(id = "tabs",
      tabPanel("Home",
               actionButton("start", "Start"),
               actionButton("kill", "Kill"),
               textOutput("monitor"),
               htmlOutput("log")
               ),
      tabPanel("Settings", settings_module_ui("settings")),
      tabPanel("Input map", value = "inputmap", editor_module_ui("inputmap")),
      tabPanel("Output map", value = "outputmap", editor_module_ui("outputmap")),
    )
  )
)

server <- function(input, output, session) {
  

  file_inputmap <- editor_module_server("inputmap")
  file_outputmap <- editor_module_server("outputmap")
  settings <- settings_module_server("settings",
                                     inputmap = file_inputmap,
                                     outputmap = file_outputmap)
  
  
  # Switch tabs on demand, required by the links in settings_module
  tabSelect <- reactive(settings$tabSelect())
  observeEvent(tabSelect(), {
    message("updating")
    req(tabSelect())
    updateTabsetPanel(session, "tabs", selected = tabSelect())
  })
  

  # Background process holder
  process <- reactiveVal(NULL)
  # Store logs from background process
  logs <- reactiveVal("")
  
  # Start process on click
  observeEvent(input$start, {
    shinyjs::disable("start")
    logs()
    settings_path <- tempfile(fileext = ".yaml")
    yaml::write_yaml(settings$settings(), settings_path)
    
    proc <- callr::r_bg(function(settings_file) { 
      library(rlang)
      message("abc")
      Sys.sleep(10)
      source("testfile.R", local = env(settings_file = settings_file)) 
      }, 
      args = list(settings_file = settings_path),
      supervise = TRUE,
      stderr = "2>&1"
    )
    process(proc)
  })
  
  procMonitor <- reactive({
    invalidateLater(1000, session)
    req(process())
    process()$is_alive()
  })
  
  observe({
    invalidateLater(1000, session)
    req(process())
    # Append to log and update editor
    logs(stringr::str_c(logs(), process()$read_output()))
    #updateAceEditor(session, "log", value=logs())
    output$log <- renderUI({ansi2html(logs())})
  })
  
  output$monitor <- renderText({ procMonitor() })

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
