#
#
downloadModuleInput <- function(id) {
  ns <- NS(id)
  
  tagList(
    textInput(ns("filename"), "Save data as", value = "secure-data.csv"),
    downloadButton(ns("save"), "Save Data")
  )
}

downloadModule <- function(input, output, session, data) {
  output$save <- downloadHandler(
    filename = function() input$filename,
    content = function(file) {
        write.csv(data(), file, row.names = FALSE)
    }
  )
}
