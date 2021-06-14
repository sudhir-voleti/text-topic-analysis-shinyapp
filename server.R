#################################################
#               Topic    Analysis             #
#################################################

shinyServer(function(input, output,session) {
  set.seed=2092014   

  dataset <- reactive({
    if (is.null(input$file)) {return(NULL)}
    else {
      
      if(file_ext(input$file$datapath)=="txt"){
        Document = readLines(input$file$datapath)
        #colnames(Document) <- c("Doc.id","Document")
        Doc.id=seq(1:length(Document))
        calib=data.frame(Doc.id,Document)
        print(input$file$name)
        return(calib)}
      else{
        Document = read.csv(input$file$datapath ,header=TRUE, sep = ",", stringsAsFactors = F)
        Document[,1] <- str_to_title(Document[,1])
        Document[,1] <- make.names(Document[,1], unique=TRUE)
        Document[,1] <- tolower(Document[,1])
        Document[,1] <- str_replace_all(Document[,1],"\\.","_")
        Document<-Document[complete.cases(Document), ]
        Document <- Document[!(duplicated(Document[,1])), ]
        rownames(Document) <- Document[,1]
        
        # colnames(Document) <- c("Doc.id","Document")
        #Doc.id=seq(1:length(Document))
        # calib=data.frame(Doc.id,Document)
        #print(input$file$name)
        
        return(Document)
      }
      
    }
  })
  
  output$up_size <- renderPrint({
    size <- dim(dataset())
    paste0("Dimensions of uploaded data: ",size[1]," (rows) X ", size[2]," (Columns)")
  })
  
  
  output$samp_data <- DT::renderDataTable({
    DT::datatable(head(dataset()),rownames = FALSE)
  })
  
  cols <- reactive({colnames(dataset())})
  y_col <- reactive({
    x <- match(input$x,cols())
    y_col <- cols()[-x]
    return(y_col)
    
  })
  
  output$id_var <- renderUI({
    print(cols())
    selectInput("x","Select ID Column",choices = cols())
  })
  
  
  output$doc_var <- renderUI({
    selectInput("y","Select Text Column",choices = y_col())
  })
  
  
  
  
dtm_tcm =  eventReactive(input$apply,{
  
  textb = dataset()[,input$y]
  ids = dataset()[,input$x]

  dtm.tcm = dtm.tcm.creator(text = textb,
                            id = ids,
                            std.clean = TRUE,
                            std.stop.words = TRUE,
                            stop.words.additional = unlist(strsplit(input$stopw,",")),
                            bigram.encoding = TRUE,
                            # bigram.min.freq = 20,
                            min.dtm.freq = input$freq,
                            skip.grams.window = 10)
  # if (input$ws == "weightTf") {

    dtm = as.matrix(dtm.tcm$dtm)#, weighting = weightTf)  

  # } 
  # 
  # if (input$ws == "weightTfIdf"){
  #   dtm = as.DocumentTermMatrix(dtm.tcm$dtm, weighting = weightTfIdf)
  # }
  
  # tcm = dtm.tcm$tcm
  dtm_tcm_obj = list(dtm = dtm)#tcm = tcm)
  return(dtm_tcm_obj)
})


output$dtm_size <- renderPrint({
  size <- dim(dtm_tcm()$dtm)
  paste0("Dimensions of DTM: ",size[1]," (rows) X ", size[2]," (Columns)")
})


wordcounts = reactive({
  
  return(dtm.word.count(dtm_tcm()$dtm))
  
}) 

output$wordcloud <- renderPlot({
  tsum = wordcounts()
  tsum = tsum[order(tsum, decreasing = T)]
  dtm.word.cloud(count = tsum,max.words = input$max,title = 'Term Frequency Wordcloud')
  
      })


ordered_dtm <- reactive({if (is.null(input$file)) {return(NULL)}
  else{
    mat1= dtm_tcm()$dtm
    a = colSums(mat1)
    b = order(-a)     # nice syntax for ordering vector in decr order  
    mat2 = mat1[,b]
    return(mat2)
    
  }
  
})

output$dtmsummary  <- DT::renderDataTable({
      if (is.null(input$file)) {return(NULL)}
  else {
    # sortedobj = dtm_tcm()$dtm[,order(wordcounts(), decreasing = T)]
    # t(sortedobj[1:10,1:10])
    temp <- ordered_dtm()[1:10,1:10]
    #  temp <- tem[1:10,1:10]
    return(temp)
  }
      })

output$dtmsummary1  <- DT::renderDataTable({
  if (is.null(input$file)) {return(NULL)}
  else {
    data.frame(Counts = wordcounts()[order(wordcounts(), decreasing = T)][1:input$max])
  }
})

#---------------

lda = reactive({
  lda = fit.lda.topics(dtm_tcm()$dtm, K= input$topic)
  return(lda)
})

output$summary <- renderPrint({
  summary(lda()$simfit)
})

output$plots2 <- renderUI({
  plot_output_list <- lapply(1:input$topic, function(i) {
    plotname <- paste("plot", i, sep="")
    plotOutput(plotname, height = 800, width = 800)
  })
  
  # Convert the list to a tagList - this is necessary for the list of items
  # to display properly.
  do.call(tagList, plot_output_list)
})

# Call renderPlot for each one. Plots are only actually generated when they
# are visible on the web page.

for (i in 1:max_plots) {
  # Need local so that each item gets its own number. Without it, the value
  # of i in the renderPlot() will be the same across all instances, because
  # of when the expression is evaluated.
  local({
    
    my_i <- i 
    plotname <- paste("plot", my_i, sep="")
    
    output[[plotname]] <- renderPlot({
      
      censored.lift = lda()$censored.lift
      theta = lda()$theta
      dtm = dtm_tcm()$dtm
      
      a0 = which(censored.lift[,my_i] > 1) # terms with lift greator than 1 for topic i
      freq = theta[a0,my_i] # Theta for terms lift greator than 1
      freq = sort(freq, decreasing = T) # Terms with higher probilities for topic i
      
      # Auto Correction -  Sometime terms in topic with lift above 1 are less than 100. So auto correction
      n = ifelse(length(freq) >= 100, 100, length(freq))
      top_word = as.matrix(freq[1:n])
      
      # SUB tcm
      # sub.tcm = tcm[colnames(tcm) %in% names(a0),colnames(tcm) %in% names(a0)]
      # SUB dtm
      sub.dtm = dtm[,colnames(dtm) %in% names(a0)]
      
      #   Plot wordcloud
      wordcloud(rownames(top_word), top_word,  scale=c(4,.2), 1,
                random.order=FALSE, random.color=FALSE, 
                colors=brewer.pal(8, "Dark2"))
      mtext(paste("Latent Topic",my_i), side = 3, line = 2, cex=2)
      
      box(lty = '11', col = 'black')
    })
  })
}


output$plots3 <- renderUI({
  plot_output_list <- lapply(1:input$topic, function(i) {
    plotname <- paste("plot1", i, sep="")
    plotOutput(plotname, height = 800, width = 800)
  })
  
  # Convert the list to a tagList - this is necessary for the list of items
  # to display properly.
  do.call(tagList, plot_output_list)
})

# Call renderPlot for each one. Plots are only actually generated when they
# are visible on the web page.

for (j in 1:max_plots) {
  # Need local so that each item gets its own number. Without it, the value
  # of i in the renderPlot() will be the same across all instances, because
  # of when the expression is evaluated.
  local({
    
    my_i1 <- j
    plotname <- paste("plot1", my_i1, sep="")
    
    output[[plotname]] <- renderPlot({
      
      censored.lift = lda()$censored.lift
      theta = lda()$theta
      dtm = dtm_tcm()$dtm
      
      a0 = which(censored.lift[,my_i1] > 1) # terms with lift greator than 1 for topic i
      sub.dtm = dtm[,colnames(dtm) %in% names(a0)]

      # PLot TCM
      distill.cog.tcm(mat1=sub.dtm, # input TCM MAT
                      mattype = "DTM",
                      title = paste0("TCM from DTM Adjacency - Topic ",my_i1), # title for the graph
                      s=input$nodes,    # no. of central nodes
                      k1 = input$connection)  # No. of Connection with central Nodes
      # mtext(paste("Term co-occurrence - Topic",my_i1), side = 3, line = 2, cex=2)
      box(lty = '11', col = 'black')
    })
  })
}

# output$score = renderTable({
  # head(lda()$theta,30)
  
 da2 = reactive({ 
  
  censored.lift = lda()$censored.lift
  theta = lda()$theta
  
  # my edit begins here
  z0 = apply(theta, 2, mean)
  theta = theta/z0   # my edit ends
  
  p = round(75/input$topic)
  words = NULL
  
  for(i in 1:input$topic){
  
  a0 = which(censored.lift[,i] > 1) # terms with lift greator than 1 for topic i
  freq = theta[a0, i] # Theta for terms lift greator than 1
  freq = sort(freq, decreasing = T) # Terms with higher probilities for topic i
  freq = freq[1:p]
  words = c(words,freq)
  }
  mat = theta[row.names(theta) %in% names(words),]
  mat = mat[order(mat[,1],mat[,2], decreasing = T),]
   mat = round(mat, 2)
  terms = row.names(mat)
  mat1 = data.frame(terms, mat)   # , terms
  colnames(mat1) = c('terms', paste0('topic_',1:input$topic))    # 'terms', 
  return(mat1)
})     # reactive da2 ends
  
# Show table:
output$score <- DT::renderDataTable({
 DT::datatable(da2(),options = list(lengthMenu = c(10, 30, 50), pageLength = 100),rownames = FALSE)
})  # my edits here
  
# }, digits = 3)   # my edit
  
da1 = reactive({
  if (is.null(input$file)) {return(NULL)}
  {
    dataset <- dataset()
    dataset[,input$x] <- as.character(dataset[,input$x])
    tb = lda()$kappa*100
    tb = data.frame(rownames(tb), round(tb, 2))   # my edit
    colnames(tb) = c(input$x,paste("Topic",1:(ncol(tb)-1)))
    
    test =  tb %>% dplyr::left_join(dataset,by = input$x)
    #test = merge(tb, dataset(), by.x =input$x, by.y= input$x, all=T)
    return(test)}
})
# Show table:
output$table <- renderDataTable({
  da1()
}, options = list(lengthMenu = c(5, 30, 50), pageLength = 30))


#---------------
output$downloadData1 <- downloadHandler(
    filename = function() { "Nokia_Lumia_reviews.txt" },
    content = function(file) {
      writeLines(readLines("data/Nokia_Lumia_reviews.txt"), file)
    }
  )

output$downloadData2 <- downloadHandler(
  filename = function() { "Topic_scores.csv" },
  content = function(file) {
    write.csv(da1(), file, row.names=F)
  }
)

})
