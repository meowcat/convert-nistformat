library(listviewer)
library(shinyAce)
source("functions.R")

processingtasks_module_ui <- function(id, info) tagList(
  fluidRow(
    column(6, 
           selectInput(NS(id, "select_task"),  label = "Task", choices = names(info)),
           actionButton(NS(id, "add"), label = "add"),
           actionButton(NS(id, "remove"), label = "remove"),
           selectInput(NS(id, "tasks_list"), selectize = FALSE, label = "Active tasks", choices = c(), size = 5),
           textOutput(NS(id, "task_info")),
           ),
    column(6, 
           aceEditor(NS(id, "task_params"), mode = "yaml", height = "150px"),
           actionButton(NS(id, "update"), label = "update"),
           )
  )
)

processingtasks_module_server<- function(id, tasks_data, info) {
  
  stopifnot(is.reactive(tasks_data))
  stopifnot(!is.reactive(info))
  moduleServer(id, function(input, output, session) {
    
    # updateSelectInput(session, "select_task", choices = names(info))
    
    observeEvent(input$add, {
      tasks_data_new <- c(tasks_data(),
                          list(list(
                            task = input$select_task,
                            params = info[[input$select_task]]$params
                          )))
      tasks_data(tasks_data_new)
    })
    
    observeEvent( input$remove, {
       tasks_data_new <- tasks_data()[-as.integer(input$tasks_list)]
       tasks_data(tasks_data_new)
    })
    
    observeEvent( tasks_data(), {
      # Update 
      updateSelectInput(
        session, "tasks_list", 
        choices = seq_along(tasks_data()) %>% set_names(map_chr(tasks_data(), "task"))
      )
      
    })
    

    observeEvent(input$tasks_list,{
      updateAceEditor(
        session, "task_params", 
        value = yaml::as.yaml(tasks_data()[[as.integer(input$tasks_list)]]$params))
    })
    
    output$task_info <- renderText({
      req(input$tasks_list)
      activeTask <- tasks_data()[[as.integer(input$tasks_list)]]
      info[[activeTask$task]]$info
    })
    
    observeEvent(
      input$update, {
        tasks_data_new <- tasks_data()
        new_params <- yaml::yaml.load(input$task_params)
        if(length(new_params) == 0)
          new_params <- NULL
        tasks_data_new[[as.integer(input$tasks_list)]]$params <- new_params
        tasks_data(tasks_data_new)
      }
    )
    

    
})}

# ui  <- fluidPage(
#   processingtasks_module_ui("tsk", process_spectra)
# )
# server <- function(input, output, session) {
#   td <- reactiveVal(list())
#   processingtasks_module_server("tsk", td, process_spectra)
# }
# shinyApp(ui, server)

