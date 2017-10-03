#
#
downloadModuleInput <- function(id) {
  ns <- NS(id)
  
  tagList(
    textInput(ns("filename"), "Archive approved data and notes as", value = "secure-data.csv"),
    downloadButton(ns("save"), "Save")
  )
}

downloadModule <- function(input, output, session, data, rnames) {
  output$save <- downloadHandler(
    filename = function() input$filename,
    content = function(file) {
      if (rnames())
        write.csv(data(), file)
      else
        write.csv(data(), file, row.names = FALSE)
    }
  )
}
