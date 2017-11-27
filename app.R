library(shiny)
library(shinyFiles)
library(fmsb)
source("UploadModule.R")
source("downloadModule.R")


ui <- fluidPage(width = 720,

    fluidRow(column(3,tags$img(src="ons-logo.png", width=90)),
             column(9, textOutput("liu"),align = "right")
             ),

    mainPanel(width = 12,
      tabsetPanel(
        tabPanel("Overview", 
                 h3("This is a PoC in R for loading data and adding metadata (including the data sensitivity framework scores) to it before uploading them to a Secure Data Lake"),
                 img(src='MPPoC-Overview.png', align = "centre", width=700),
                 br(),
                 h6("Version 1.8 by Neville de Mendonca & Mark Craddock")
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
                           uploadModuleInput("AVfile"),br(),
                           downloadModuleInput("AVfile")
                     )
                     ),
                    mainPanel( 
                      img(src='AVscan.jpg', align = "centre", width=400)
                    )
                )
       ),
        tabPanel("Preview Data",    
          h3("Select a data file to review and annnotate from the DMZ"),
          sidebarLayout(
            sidebarPanel(
            uploadModuleInput("datafile")
          ),   mainPanel(verbatimTextOutput("summary"),dataTableOutput("table")))),
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
        tabPanel("Add DSF", 
          h3("Assign the Data Sensitivity Framework scores for this data set"),
          sidebarLayout(
            sidebarPanel(
            
            tagList(
              selectInput("sensitivity", "Sensitivity:", 
                          list("Open Public"=0,"Commercial"=4,"Private Personal"=8, "Secret"=12)),
              selectInput("indentifies", "Identifies:", 
                          list("Non Personal"=0,"Groups"=6,"Individuals"=12)),  
              selectInput("granularity", "Granularity:",
                          list("Population"=0,"Sample"=4,"Record"=8, "Field Variable"=12)),
              selectInput("recency", "Recency:",
                          list("Historical"=0,"Periodic"=6,"Real-time"=12)),   
              selectInput("reliability", "Reliability:",
                          list("Complete"=0,"Substantial"=4,"Patchy"=8, "Incomplete"=12)),  
              selectInput("release", "Release:",
                          list("Open"=0,"Restricted"=6,"Closed"=12)),                              
              selectInput("audience", "Audience:",
                          list("Public"=0, "Third Parties by type"=4, "Closed Group"=8, "Named"=12))
                )  
           ),
              mainPanel(
                plotOutput('radarPlot',width = "450px")  # height="auto")
              )
           )
        ),

        tabPanel("Ingest Data",    
                 h3("Upload dataset, metadata and data sensitivity information to Data centre"),
                 sidebarLayout(
                   sidebarPanel(
                     tagList(
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
  AVfile <- callModule(uploadModule, "AVfile")
  callModule(downloadModule, "AVfile", AVfile)
  
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
    
    # pfcol=rgb(0.2,0.5,0.5,0.5)
    colors_border=c( rgb(0.8,0.2,0.5,0.9) , rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
    colors_in=c(  rgb(0.7,0.5,0.1,0.4), rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4)  )
    
    radarchart(mpdat, axistype=0, seg=3, plty=1, pcol=colors_border, pfcol=colors_in, vlabels=mpnames,
                          title="ONS DSF Radar v1.5", vlcex=1.4)},
    height = function() {
      session$clientData$output_radarPlot_width
    }
 ) 

  callModule(downloadModule, "download", datafile)
  
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
   switch(input$odataset,
        "Metadata" =c(
          MD_user=as.character( c(input$loginname)),
          DSF_coded_sensitivity=as.integer( c(input$sensitivity)),
          DSF_coded_indentifies=as.integer( c(input$indentifies)),
          DSF_coded_granularity=as.integer( c(input$granularity)),
          DSF_coded_recency=as.integer( c(input$recency)),
          DSF_coded_reliability=as.integer( c(input$reliability)),
          DSF_coded_release=as.integer( c(input$release)),
          DSF_coded_audience=as.integer( c(input$audience)),
          MD_Source=as.character( c(input$MD_Source)),
          MD_Desc=as.character( c(input$MD_Desc)),
          MD_Sender=as.character( c(input$MD_Sender)),
          MD_Class=as.character( c(input$MD_Class)),
          MD_Complete=as.character( c(input$MD_Complete))
          
        ) 
     )
  })
  
  # Table of Metadata for the selected dataset ----
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
    }
  )

}

shinyApp(ui, server)