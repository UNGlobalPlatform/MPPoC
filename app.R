library(shiny)
library(fmsb)
source("UploadModule.R")
source("downloadModule.R")


ui <- fluidPage(width = 1000,

    fluidRow(column(3,tags$img(src="ons-logo.png", width=180)),
             column(9, textOutput("liu"),align = "right")
             ),
    br(),
  
    mainPanel(width = 12,
      tabsetPanel(
        tabPanel("Overview", 
                 h3("This is a PoC in R for loading data and adding metadata and Material Propretries to it before uploading them to a Secure Data Lake"),
                 img(src='MPPoC-Overview.png', align = "centre", width=800),
                 br(),
                 h6("Version 1.4 by Neville de Mendonca & Mark Craddock")
                ),
        tabPanel("Login", 
                 h3("Please login, to record who added the annotations and authorised the upload"),
                 sidebarLayout(
                   sidebarPanel(
                     tagList(                 
                        textInput("loginname", label = "Name"),
                        passwordInput("loginpwd", label = "Password"),
                        br(),
                        actionButton("loginbtn", "Log in")
                        )
                 ),
                 mainPanel( 
                   img(src='login-panel.jpg', align = "centre", width=400) 
                 )
          )
        ),
        tabPanel("AV Check", 
                   h3("Upload a file into the DMZ for AV scanning"),
                   sidebarLayout(
                   sidebarPanel(
                     tagList(
                           fileInput("file", label = "select file")
                      )
                     ),
                    mainPanel( 
                      # width = 9, align = "centre",
                      img(src='AVscan.jpg', align = "centre", width=400)
                    )
                )
       ),
        tabPanel("Preview Data",    
          h3("Select the data file you are reviewing from the DMZ"),
          sidebarLayout(
            sidebarPanel(
            uploadModuleInput("datafile")
          ),   mainPanel(dataTableOutput("table")))),
        tabPanel("Add Metadata",
                 h3("You may add notes on the data set being uploaded below"),
                 sidebarLayout(
                   sidebarPanel(
                     tagList(
                       textInput("MD_Source", label = "Source"),
                       textInput("MD_Desc", label = "Description"),
                       textInput("MD_Sender", label = "Sender Contact"),
                       textInput("MD_Class", label = "Classification"),
                       textInput("MD_Complete", label = "Complete")
                     )
                   ),
                  mainPanel( 
                   img(src='UN-metis.PNG', align = "centre", width=400) 
                  )
              )
        ),
        tabPanel("Material Properties", 
          h3("Assign the Material Properties for this data set"),
          sidebarLayout(
            sidebarPanel(
            
            tagList(
              selectInput("sensitivity", "Sensitivity:", 
                          list("Open Public"=0,"Commercial"=4,"Private Personal"=8, "Secret"=12)),
#              div(p(actionLink("showsensitivity", "", icon=(icon("question-circle", class = NULL, lib = "font-awesome"))),align="right",color="blue")),
              
              selectInput("indentifies", "Identifies:", 
                          list("Non Personal"=0,"Groups"=6,"Individuals"=12)),  
#              div(p(actionLink("showindentifies", "", icon=(icon("question-circle", class = NULL, lib = "font-awesome"))),align="right",color="red")),
              
              selectInput("granularity", "Granularity:",
                          list("Population"=0,"Sample"=4,"Record"=8, "Field Variable"=12)),
#              div(p(actionLink("showgranularity", "", icon=(icon("question-circle", class = NULL, lib = "font-awesome"))),align="right")),
              
              selectInput("recency", "Recency:",
                          list("Historical"=0,"Periodic"=6,"Real-time"=12)),   
#              div(p(actionLink("showrecency", "", icon=(icon("question-circle", class = NULL, lib = "font-awesome"))),align="right")),
              
              selectInput("reliability", "Reliability:",
                          list("Incomplete"=0,"Patchy"=4,"Substantial"=8, "Complete"=12)),  
#              div(p(actionLink("showreliability", "", icon=(icon("question-circle", class = NULL, lib = "font-awesome"))),align="right")),
              
              selectInput("release", "Release:",
                          list("Closed"=0,"Restricted"=6,"Open"=12)),                              
#              div(p(actionLink("showrelease", "", icon=(icon("question-circle", class = NULL, lib = "font-awesome"))),align="right")),
              
              selectInput("audience", "Audience:",
                          list("Named"=0,"Closed Group"=4,"Third Parties by type"=8, "Public"=12))
#              div(p(actionLink("showaudience", "", icon=(icon("question-circle", class = NULL, lib = "font-awesome"))),align="right",color="blue"))
              
                )  
           ),
              mainPanel(
                plotOutput('radarPlot',height="auto")
              )
           )
        ),

        tabPanel("Ingest Data",    
                 h3("Upload dataset, metadata and material properties information to Data centre"),
                 sidebarLayout(
                   sidebarPanel(
                     tagList(
                       checkboxInput("row.names", "Append row names"),
                       downloadModuleInput("download"),
                       hr(),
                       selectInput("odataset", "Save Metadata:",
                                   choices = c("Metadata")),
                       downloadButton("downloadData", "Save Metadata")
                       
                    )
                   ),
                   mainPanel(
                     img(src='DataCentre.jpg', align = "centre", width=400), 
                     dataTableOutput("otable")
                     
                   )
                )
        )
      )
    )
  )

  

server <- function(input, output, session) {
  

  # record who is logged in and when
  
  output$liu <- renderText({
    req(input$loginbtn)
    paste("Logged in as :",isolate(input$loginname), " at ", format(Sys.time(),"%H:%M:%S"), " on ", format(Sys.Date(),"%a %d %b %Y"))
  })
  

  # upload a file for review (from DMZ?)  
  datafile <- callModule(uploadModule, "datafile")
  
  output$table <- renderDataTable({
    datafile()
  })
  
  # Material Properties
  
  # Reactive expression to compose a data frame containing all of the values
  
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
    
    # This populates dat with the current values from UI drop down selectors

    mpnames <- c("Sensitivity", "Indentifies", "Granularity", "Recency", "Reliability", "Release", "Audience")
    
    dat <- data.frame( 
      sensitivity=as.integer( c(input$sensitivity)),
      indentifies=as.integer( c(input$indentifies)),
      granularity=as.integer( c(input$granularity)),
      recency=as.integer( c(input$recency)),
      reliability=as.integer( c(input$reliability)),
      release=as.integer( c(input$release)),
      audience=as.integer( c(input$audience))
    )
      
    # Merges actual data with min/max data for mp plot
    mpdat <- rbind(maxmin,dat)
    
    radarchart(mpdat, axistype=0, seg=3, plty=1, pfcol=rgb(0.2,0.5,0.5,0.5), vlabels=mpnames,
                          title="ONS MPoD Radar v1.4", vlcex=1.4)},
    height = function() {
      session$clientData$output_radarPlot_width
    }
 ) 

  callModule(downloadModule, "download", datafile, reactive(input$row.names))
  
#nt
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
   switch(input$odataset,
        "Metadata" =c(
          MD_user=as.character( c(input$loginname)),
          MP_coded_sensitivity=as.integer( c(input$sensitivity)),
          MP_coded_indentifies=as.integer( c(input$indentifies)),
          MP_coded_granularity=as.integer( c(input$granularity)),
          MP_coded_recency=as.integer( c(input$recency)),
          MP_coded_reliability=as.integer( c(input$reliability)),
          MP_coded_release=as.integer( c(input$release)),
          MP_coded_audience=as.integer( c(input$audience)),
          MD_Source=as.character( c(input$MD_Source)),
          MD_Desc=as.character( c(input$MD_Desc)),
          MD_Sender=as.character( c(input$MD_Sender)),
          MD_Class=as.character( c(input$MD_Class)),
          MD_Complete=as.character( c(input$MD_Complete))
          
        ) 
     )
  })
  
  # Table of selected dataset ----
  output$otable <- renderTable({
    datasetInput()
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$odataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = TRUE)
#      write.csv(datasetInput(), col.names=c("name","value"),file, row.names = TRUE)
    }
  )
  
 

   
    # Material Properties info buttons commented out whilst new descriptive text is prepared
  
  
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

}

shinyApp(ui, server)