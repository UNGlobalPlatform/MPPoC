library(shiny)
library(fmsb)
source("UploadModule.R")
source("downloadModule.R")


ui <- fluidPage(width = 1000,

    tags$img(src="ons-logo.png", width=180),
    br(),
  
    mainPanel(width = 12,
      tabsetPanel(
        tabPanel("Overview", h3("This is a PoC in R for loading data and adding metadata and Material Propretries to it before uploading them to a Secure Data Lake"),img(src='MPPoC-Overview.png', align = "centre", width=800) ),
        tabPanel("Login", textInput("text", label = h3("Login")),passwordInput("text", label = h3("Pasword"))),
        tabPanel("AV Check", fileInput("file", label = h3("Upload a file into the DMZ for AV scanning"))),
        tabPanel("Preview Data",    sidebarLayout(
          sidebarPanel(
            uploadModuleInput("datafile"),
            tags$hr(),
            checkboxInput("row.names", "Append row names")
            # downloadModuleInput("download")
          ),   mainPanel(dataTableOutput("table")))),
        tabPanel("Add Metadata",
                 textInput("text", label = h3("Source")),
                 textInput("text", label = h3("Description")),
                 textInput("text", label = h3("Sender Contact")),
                 textInput("text", label = h3("Classification")),
                 textInput("text", label = h3("Complete"))
        ),
        tabPanel("Material Properties", 
          sidebarLayout(

            sidebarPanel(
            
            tagList(
              selectInput("sensitivity", "Sensitivity:", 
                          list("Open Public"=0,"Commercial"=4,"Private Personal"=8, "Secret"=12)),
              div(p(actionLink("showsensitivity", "", icon=(icon("question-circle", class = NULL, lib = "font-awesome"))),align="right",color="blue")),
              
              selectInput("indentifies", "Identifies:", 
                          list("Non Personal"=0,"Groups"=6,"Individuals"=12)),  
              div(p(actionLink("showindentifies", "", icon=(icon("question-circle", class = NULL, lib = "font-awesome"))),align="right",color="red")),
              
              selectInput("granularity", "Granularity:",
                          list("Population"=0,"Sample"=4,"Record"=8, "Field Variable"=12)),
              div(p(actionLink("showgranularity", "", icon=(icon("question-circle", class = NULL, lib = "font-awesome"))),align="right")),
              
              selectInput("recency", "Recency:",
                          list("Historical"=0,"Periodic"=6,"Real-time"=12)),   
              div(p(actionLink("showrecency", "", icon=(icon("question-circle", class = NULL, lib = "font-awesome"))),align="right")),
              
              selectInput("reliability", "Reliability:",
                          list("Incomplete"=0,"Patchy"=4,"Substantial"=8, "Complete"=12)),  
              div(p(actionLink("showreliability", "", icon=(icon("question-circle", class = NULL, lib = "font-awesome"))),align="right")),
              
              selectInput("release", "Release:",
                          list("Closed"=0,"Restricted"=6,"Open"=12)),                              
              div(p(actionLink("showrelease", "", icon=(icon("question-circle", class = NULL, lib = "font-awesome"))),align="right")),
              
              selectInput("audience", "Audience:",
                          list("Named"=0,"Closed Group"=4,"Third Parties by type"=8, "Public"=12)),
              div(p(actionLink("showaudience", "", icon=(icon("question-circle", class = NULL, lib = "font-awesome"))),align="right",color="blue"))
              
                ) #tl
           ),
              mainPanel(
                plotOutput('radarPlot',height="800px", width="800px")
                )
              )
        ),

        tabPanel("Load Library",    
             downloadModuleInput("download")
           )
      )
    )
  )

  

server <- function(input, output, session) {
  
  datafile <- callModule(uploadModule, "datafile")
  
  output$table <- renderDataTable({
    datafile()
  })
  
  # Reactive expression to compose a data frame containing all of the values
  #sliderValues <- reactive({
  output$radarPlot <- renderPlot({
    
    # Data must be given as the data frame, where the first cases show maximum.
    # This section is create the data frame with the min/max numbers for each row
    maxmin <- data.frame(
      sensitivity=c(12, 0),
      indentifies=c(12, 0),
      granularity=c(12, 0),
      recency=c(12, 0),
      reliability=c(12, 0),
      release=c(12, 0),
      audience=c(12, 0)
    )
    
    # This populates dat with random numbers for the actual value
    dat <- data.frame(
      sensitivity=as.integer( c(input$sensitivity)),
      indentifies=as.integer( c(input$indentifies)),
      granularity=as.integer( c(input$granularity)),
      recency=as.integer( c(input$recency)),
      reliability=as.integer( c(input$reliability)),
      release=as.integer( c(input$release)),
      audience=as.integer( c(input$audience))
    )
    
    # Merges actual data with min/max data
    dat <- rbind(maxmin,dat)
    
    
    radarchart(dat, axistype=0, seg=3, plty=1, pfcol=rgb(0.2,0.5,0.5,0.5), vlabels=c("Sensitivity", "Indentifies",
                                                                                     "Granularity", "Recency", "Reliability", "Release", "Audience"),
               title="ONS MPoD Radar v1.1", vlcex=1.4)
  })
  
  observeEvent(input$showindentifies, {
    showModal(modalDialog(
      title = "Indentifies",
      "This could be where we will explain in detail what is meant by Indentifies and the catagories of Non-Personal, Groups and Individuals",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  observeEvent(input$showgranularity, {
    showModal(modalDialog(
      title = "Granularity",
      "This could be where we will explain in detail what is meant by Granularity and the catagories of Population, Sample, Record and Field Variable",
      easyClose = TRUE,
      footer = NULL
    ))
  })    
  observeEvent(input$showrecency, {
    showModal(modalDialog(
      title = "Recency",
      "This could be where we will explain in detail what is meant by Recency and the catagories of Historical, Periodic and Real-time",
      easyClose = TRUE,
      footer = NULL
    ))
  })    
  observeEvent(input$showreliability, {
    showModal(modalDialog(
      title = "Reliability",
      "This could be where we will explain in detail what is meant by Reliability and the catagories of Incomplete, Patchy, Substantial and Complete",
      easyClose = TRUE,
      footer = NULL
    ))
  })    
  observeEvent(input$showrelease, {
    showModal(modalDialog(
      title = "Release",
      "This could be where we will explain in detail what is meant by Sensitivity and the catagories of Closed, Restricted and Open",
      easyClose = TRUE,
      footer = NULL
    ))
  })    
  observeEvent(input$showaudience, {
    showModal(modalDialog(
      title = "Audience",
      "This could be where we will explain in detail what is meant by Audience and the catagories of Closed Group, Third Parties by type and Public",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  callModule(downloadModule, "download", datafile, reactive(input$row.names))
}

shinyApp(ui, server)