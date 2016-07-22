####################################################
#      Topic Analysis                              #
####################################################


library("shiny")


    shinyUI(fluidPage(
      
    tags$head(includeScript("google_analytics.js")),
      
    titlePanel("Topic Models"),
  
# Input in sidepanel:
    sidebarPanel(
  
    fileInput("file", "Upload text file"),
    
    textInput("stopw", ("Enter stop words seperated by comma(,)"), value = "will,can"),
    
    sliderInput("freq", "Minimum Frequency in Wordcloud:", min = 1,  max = 50, value = 4),
    
    sliderInput("max",  "Maximum Number of Words in Wordcloud:", min = 1,  max = 300,  value = 80),  

    numericInput("tdmfreq", "Minimum frequency of terms for Topic Model:", 2),
    
    h6(div(textOutput("caption1"),style = "color:Blue")),
    
    h6(div(textOutput("caption2"))),
    
    numericInput("topic", "Number of Topics to fit:", 2),
    
    submitButton(text = "Apply Changes", icon("refresh"))
      
    ),
  
# Main Panel:
    mainPanel( 
      

      tabsetPanel(type = "tabs",
#
                        tabPanel("Overview",h4(p("How to use this App")),
                                 
                        p("To use this app you need a document corpus in txt file format. Make sure each document is seperated from another document with a new line character.
                          To fit topic models in your text corpus, click on Browse in left-sidebar panel and upload the txt file. Once the file is uploaded it will fit 
                          the topic models with default inputs and accordingly results will be displayed in various.", align = "justify"),
                        p("If you wish to change the input, modify the input in left side-bar panel and click on Apply changes. Accordingly results in other tab will be refeshed
                          ", align = "Justify"),
                        h5("Note"),
                        p("You might observe no change in the outputs after clicking 'Apply Changes'. Wait for few seconds. As soon as all the cumputations
                          are over in back-end results will be refreshed",
                          align = "justify"),
                        verbatimTextOutput("tmp"),br())
,

                         tabPanel("Corpus Word Cloud",plotOutput("wordcloud")),
#                         
                         tabPanel("Topic Model - Summary",verbatimTextOutput("summary")),
                         tabPanel("Topics Wordcloud",uiOutput("plots2")),
                         tabPanel("Topics Co-occurrence",uiOutput("plots3")),
                         # tabPanel("Topics eta values",tableOutput("summary2")),
                        
#                         
                         tabPanel("Data with Topic Proportions",dataTableOutput("table"))
#                         
                 )
              )
   
    )
    )