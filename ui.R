#################################################
#               Topic  Analysis             #
#################################################

library(shiny)
library(text2vec)
library(tm)
library(tokenizers)
library(wordcloud)
library(slam)
library(maptpx)

shinyUI(fluidPage(
  
  title = "Text Topic Analysis",
  titlePanel(title=div(img(src="logo.png",align='right'),"Text Topic Analysis")),
  # Input in sidepanel:
  sidebarPanel(
    
    fileInput("file", "Upload text file"),
    uiOutput('id_var'),
    uiOutput("doc_var"),
    
    textInput("stopw", ("Enter stop words separated by comma(,)"), value = "will,can"),
    
    textInput('fname',label = "Enter Topic Name (seperated by comma)"),
    
    
    # selectInput("ws", "Weighing Scheme", 
    #             c("weightTf","weightTfIdf"), selected = "weightTf"), # weightTf, weightTfIdf, weightBin, and weightSMART.
    
    sliderInput("freq", "Minimum Frequency in DTM:", min = 1,  max = 50, value = 2),
    
    sliderInput("max",  "Maximum Number of Words in Wordcloud:", min = 1,  max = 300,  value = 100),  
    
    numericInput("topic", "Number of Topics to fit:", 2),
    
    numericInput("nodes", "Number of Central Nodes in co-occurrence graph", 4),
    numericInput("connection", "Number of Max Connection with Central Node", 5),
    
    actionButton(inputId = "apply",label = "Apply Changes", icon("refresh"))    
  ),
  
  # Main Panel:
  mainPanel( 
    
    
    tabsetPanel(type = "tabs",
                #
                tabPanel("Overview & Example Dataset",h4(p("How to use this App")),
                         a(href="https://www.youtube.com/watch?v=GilUJqlvTgE","Youtube Link for App Navigation"),
                         p("To use this app you need a document corpus in txt file format. Make sure each document is separated from another document with a new line character.
                           To do basic Text Analysis in your text corpus, click on Browse in left-sidebar panel and upload the txt file. Once the file is uploaded it will do the computations in 
                            back-end with default inputs and accordingly results will be displayed in various tabs.", align = "justify"),
                         p("If you wish to change the input, modify the input in left side-bar panel and click on Apply changes. Accordingly results in other tab will be refreshed
                           ", align = "Justify"),
                         h5("Note"),
                         p("You might observe no change in the outputs after clicking 'Apply Changes'. Wait for few seconds. As soon as all the computations
                           are over in back-end results will be refreshed",
                           align = "justify"),
                          #, height = 280, width = 400
                         verbatimTextOutput("start"),
                         h4(p("Download Sample text file")), 
                         downloadButton('downloadData1', 'Download Nokia Lumia reviews TXT file'),br(),br(),
                         downloadButton('downloadData3', 'Download Uber Reviews CSV file'),br(),br(),
                         downloadButton('downloadData4', 'Download Airline Tweets CSV file'),br(),br(),
                         downloadButton('downloadData03', 'Download Mission Visions  CSV file'),br(),br(),
                         downloadButton('downloadData5', 'Download Indian Patents Act 1970 PDF file'),br(),br(),
                         p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
                         img(src = "example1.png")
                         ),
                tabPanel("Data Summary",
                         h4("Uploaded data size"),
                         verbatimTextOutput("up_size"),
                         h4("Sample of uploaded datasest"),
                         DT::dataTableOutput("samp_data")
                         ),
                tabPanel("TDM & Word Cloud",
                         h4("DTM Size"),
                         verbatimTextOutput("dtm_size"),
                         hr(),
                         h4("Term Document Matrix [1:10,1:10]"),
                         DT::dataTableOutput("dtmsummary"),
                         hr(),
                         h4("Word Cloud"),
                         plotOutput("wordcloud",height = 700, width = 700),
                         hr(),
                         h4("Weights Distribution of Wordcloud"),
                         DT::dataTableOutput("dtmsummary1")),
                
                #tabPanel("Topic Model - Summary",verbatimTextOutput("summary")),
                tabPanel("Topics Wordcloud",uiOutput("plots2")),
                tabPanel("Topics Co-occurrence",uiOutput("plots3")),
                # tabPanel("Topics eta values",tableOutput("summary2")),
                
                #                         
                tabPanel("Token-Topic Loadings",h4("Top terms for each topic"), 
                         DT::dataTableOutput("score")),
                
                tabPanel("Topic Loadings simplified", h4("Top 20 terms for each topic"), 
                         tableOutput("outp_tbl")),
                
                tabPanel("Topic Scores as Doc Proportions",br(),br(),
                         downloadButton('downloadData2', 'Download Topic Proportions file (Works only in browser)'), br(),br(),
                         dataTableOutput("table"))
                
                         )
           )
       )
    )

