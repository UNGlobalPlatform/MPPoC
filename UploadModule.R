uploadModuleInput <- function(id) {
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"), "Select a file"),
    checkboxInput(ns("heading"), "Has header row",value = TRUE)
  )
}

uploadModule <- function(input, output, session, ...) {
  
  userFile <- reactive({
    # If no file is selected, don't do anything
    req(input$file)
  })
  
  # The user's data, parsed into a data frame
  reactive({
    read.csv(userFile()$datapath,
             header = input$heading,
             ...)
  })
}