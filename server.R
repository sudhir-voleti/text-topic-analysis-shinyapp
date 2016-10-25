####################################################
#      Topic Analysis                              #
####################################################


library(shiny)
library(tm)
library(RWeka)
library(maptpx)
library(wordcloud)
library(stringr)
library(igraph)

shinyServer(function(input, output,session) {
  
  distill.cog = function(dtm1, # input dtm
                         title, # title for the graph
                         s,    # no. of central nodes
                         k1){  # max no. of connections  
    
    mat = as.matrix((dtm1))  # input dtm here
    
    mat1 = t(mat) %*% mat    # build 1 mode term term matrix
    
    # diag(mat1) =  0 
    
    #  mat1[1:10,1:6]   # view a few rows n cols
    
    a = colSums(mat1)  # collect colsums into a vector obj a
    
    b = order(-a)     # nice syntax for ordering vector in decr order  
    
    mat2 = mat1[b,b]  # 
    
    #  mat2[1:10,1:6]
    
    diag(mat2) =  0
    
    ## +++ go row by row and find top k adjacencies +++ ##
    
    wc = NULL
    
    for (i1 in 1:s){ 
      
      thresh1 = mat2[i1,][order(-mat2[i1, ])[k1]]
      
      mat2[i1, mat2[i1,] < thresh1] = 0   # wow. didn't need 2 use () in the subset here.
      
      mat2[i1, mat2[i1,] > 0 ] = 1
      
      word = names(mat2[i1, mat2[i1,] > 0])
      
      mat2[(i1+1):nrow(mat2), match(word,colnames(mat2))] = 0
      
      wc = c(wc,word)
      
    } # i1 loop ends
    
    
    mat3 = mat2[match(wc, colnames(mat2)), match(wc, colnames(mat2))]
    
    ord = colnames(mat2)[which(!is.na(match(colnames(mat2), colnames(mat3))))]  # removed any NAs from the list
    
    mat4 = mat3[match(ord, colnames(mat3)), match(ord, colnames(mat3))]
    
    graph <- graph.adjacency(mat4, mode = "undirected", weighted=T)    # Create Network object
    
    graph = simplify(graph)  
    
    V(graph)$color[1:s] = "green"
    
    V(graph)$color[(s+1):length(V(graph))] = "pink"
    
    # plot(graph)#,layout=layout.lgl)
    
    plot(graph, layout=layout.kamada.kawai, main = title)
    
    # title(main = paste("Top Words used in review -",name))
    
  }  
  
  set.seed=2092014   
  
    dataset <- reactive({
    
    if (is.null(input$file)) {return(NULL)}
    else {
      
      Document = readLines(input$file$datapath)
      Document = iconv(Document, "latin1", "ASCII", sub="")
      x = Document
      
      if (length(x) < 2) {
      x2 = paste(x, collapse=" ")
      x2 = unlist(str_split(x2,"\\s"))
      x2 = x2[(x2 != "")]
      
      x3 = character(0)
      i = 1 ; j = 25
      for (k in 1:round(length(x2))) {        
        if (j+25 > length(x2)) {j = length(x2)}         
        temp = paste(x2[i:j], collapse = " ")        
        i = i+25 ;j = j+25         
        x3 = c(x3, temp)        
        if (j == length(x2)+25 ) break
      }
      Document = as.character(x3)
      }
      
      Doc.id=seq(1:length(Document))
      calib=data.frame(Doc.id,Document)
      return(calib)}
      
       })
     
    tdm <- reactive({
      
      if (is.null(input$file)) {return(NULL)}
      else {
        
      calib = (dataset())
      text  = as.character(calib$Document)      
      
      progress <- shiny::Progress$new(session, min=1, max=4)
      on.exit(progress$close())
      
      progress$set(value = 1)
      progress$set(message = 'Text Cleaning in progress',
                   detail = 'This may take a while...')
      
      
      x  =  gsub("<.*?>", "", text)                  # regex for removing HTML tags
      x  =  gsub("[^[:alnum:]///' ]", " ", x)     # keep only alpha numeric 
      x  =  iconv(x, "latin1", "ASCII", sub="")   # Keep only ASCII characters
      x  =  tolower(x)                          # convert to lower case characters
      x  =  removePunctuation(x)                # removing punctuation marks
      x  =  removeNumbers(x)                    # removing numbers
      x  =  stripWhitespace(x)                  # removing white space
      x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
      
      text  =  x;  rm(x)
      
      stp_word1 = stopwords('english')
      stp_word2 = readLines("data/stopwords.txt")
      comn  = unique(c(stp_word1, stp_word2))
      stp_word = unique(c(gsub("'","",comn),comn))
      sto = unique(c(stp_word,unlist(strsplit(input$stopw,","))))
      
      progress$set(value = 2)
      
      myCorpus = Corpus(VectorSource(text))
      myCorpus = tm_map(myCorpus, tolower)
      myCorpus = tm_map(myCorpus, removePunctuation)
      myCorpus = tm_map(myCorpus, removeNumbers)
      myCorpus = tm_map(myCorpus, removeWords,c(sto))
      myCorpus = tm_map(myCorpus, stripWhitespace)   # removes white space
      myCorpus = as.character(unlist(myCorpus))
      
      x1 = Corpus(VectorSource(myCorpus))
      
      progress$set(message = 'Bigram creation in progress',
                   detail = 'This may take a while...')
      progress$set(value = 3)
      
      
      ngram <- function(x1) NGramTokenizer(x1, Weka_control(min = 2, max = 2))  
      
      tdm0 <- TermDocumentMatrix(x1, control = list(tokenize = ngram,
                                                    tolower = TRUE, 
                                                    removePunctuation = TRUE,
                                                    removeNumbers = TRUE,
                                                    stopwords = TRUE ))
      #stemDocument = TRUE ))    # patience. Takes a minute.
      
      tdm = tdm0; rm('tdm0')
      
      a1 = apply(tdm, 1, sum)  
      a2 = ((a1 > 1))
      tdm.new = tdm[a2, ]
      rm('a1','a2','tdm')
      
      # remove blank documents (i.e. columns with zero sums)
      a0 = NULL; 
      for (i1 in 1:ncol(tdm.new)){ if (sum(tdm.new[, i1]) == 0) {a0 = c(a0, i1)} }
      length(a0)    # no. of empty docs in the corpus
      if (length(a0) >0) { tdm.new1 = tdm.new[, -a0]} else {tdm.new1 = tdm.new};
      
      rm('a0','tdm.new')
      dim(tdm.new1)    # reduced tdm
      x1mat = t(tdm.new1)    # don't do tfidf, not mentioned anywhere for topic modeling.
      dim(x1mat);  	# store[i1, 5] = ncol(x2mat);
      
      test = colnames(x1mat); 
      test1 = gsub(" ",".", test);  # replace spaces with dots
      colnames(x1mat) = test1
      
      ## build quick wordcloud of 50 most freq terms
      
      a11 = apply(x1mat, 2, sum)
      a12 = order(a11, decreasing = T)
      a13 = as.matrix(a11[a12])
      
      #x1 = tm_map(x1, stripWhitespace)
      x1 = unlist(lapply(x1, content)) 
      for (i in 1:nrow(a13)){    
        focal.term = gsub("\\.", " ", rownames(a13)[i])
        replacement.term = gsub(" ", "-", focal.term)
        replacement.term=paste("",replacement.term,"")
        x1 = gsub(focal.term, replacement.term, x1)  
        
      }	# now, our x corpus has the top 400 bigrams encoded as unigrams
      
      progress$set(message = 'TDM creation in progress',
                   detail = 'This may take a while...')
      progress$set(value = 4)
      
      x1 = Corpus(VectorSource(x1))    # Constructs a source for a vector as input
      tdm = TermDocumentMatrix(x1)
      colnames(tdm) = calib$Doc.id
      
      return(tdm)
      }
    })
    
    output$wordcloud <- renderPlot({
      if (is.null(input$file)) {return(NULL)}
      else {
        m = as.matrix((tdm()))
        v = sort(rowSums(m), decreasing = TRUE)
        wordcloud(names(v), v, scale=c(4,0.5),input$freq, max.words=input$max,colors=brewer.pal(8, "Dark2"))
      }
    })
    
    x2mat <- reactive({
      
      if (is.null(input$file)) {return(NULL)}
      else {
        
        progress <- shiny::Progress$new(session, min=5, max=6)
        on.exit(progress$close())
        
        progress$set(message = 'TDM creation in progress',
                     detail = 'This may take a while...')
        progress$set(value = 5)
        
      tdm = (tdm())
      a1 = apply(tdm, 1, sum)
      a2 =((a1 >= input$tdmfreq))
      tdm.new = tdm[a2, ]
      
      # remove blank documents (i.e. columns with zero sums)
      a0 = NULL; 
      for (i1 in 1:ncol(tdm.new)){ if (sum(tdm.new[, i1]) == 0) {a0 = c(a0, i1)} }
      length(a0)    # no. of empty docs in the corpus
      if (length(a0) >0) { tdm.new1 = tdm.new[, -a0]} else {tdm.new1 = tdm.new};
      
      dim(tdm.new1)		# reduced tdm
      x2mat = t(tdm.new1)		# don't do tfidf, not mentioned anywhere for topic modeling.
      dim(x2mat);		# store[i1, 5] = ncol(x2mat);
      
      test = colnames(x2mat); 
      test1 = gsub(" ",".", test);  # replace spaces with dots
      colnames(x2mat) = test1
      progress$set(value = 5)
      return(x2mat) 
      }
      })

#   Optimal Topics by BIC 
    output$caption1 <- renderText({
    if (is.null(input$file)) {return(NULL)}
    else {
      progress <- shiny::Progress$new(session, min=7, max=7)
      on.exit(progress$close())
      
      progress$set(message = 'Finding optimal topics',
                   detail = 'This may take a while...')
      progress$set(value = 7)
      
      
    K=6
    x2mat = (x2mat())
    summary(simselect <- topics(x2mat, K=K+c(-4:4)), nwrd=0)
    return(paste("Note - Optimal Topics from Log Bayes Factor for selected Term Document Matrix Should be:",simselect$K,""))
  }
  
  })

    simfit <- reactive({ 
      
      progress <- shiny::Progress$new(session, min=8, max=8)
      on.exit(progress$close())
      
      progress$set(message = 'Fitting K topics Model',
                   detail = 'This may take a while...')
      progress$set(value = 8)
      
    summary(simfit <- topics((x2mat()),  K=input$topic, verb=2),nwrd = 12 )
    return(simfit)
    })

    output$summary <- renderPrint({
  summary(simfit())
  })

    lift = reactive({
    theta = (simfit())$theta
    x2mati = (x2mat())
    lift = theta*0;  sum1 = sum(x2mati)
  
    for (i in 1:nrow(theta)){  
    for (j in 1:ncol(theta)){
      
      ptermtopic = 0; pterm = 0;
      
      ptermtopic = theta[i, j]
      
      pterm = sum(x2mati[,i])/sum1
      
      lift[i, j] = ptermtopic/pterm
      
    }
    }
    return(lift)
    })

    theta = reactive({
      theta = as.matrix((simfit())$theta)
      return(theta)
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
      freq = as.matrix((theta())[(match(rownames((lift())[((lift())[,my_i] > 1),]),rownames((theta())))),][,my_i])
      freq = as.matrix(freq[(order(freq[,1], decreasing=T)),])
      if (nrow(freq) >= 40) {n = 40}
      else {n = nrow(freq)}
      top_word = as.matrix(freq[1:n,])
      #plot.new()
      title(main =paste("Latent Topic",my_i))
      wordcloud(rownames(top_word), top_word,  scale=c(4,0.5), 1, , random.order=FALSE, random.color=FALSE, colors=brewer.pal(8, "Dark2"));
      #title(main =paste("Latent Topic",my_i))
      mtext(paste("Latent Topic",my_i), side = 3, line = 2, cex=2)
      box(lty = '11', col = 'black')
    })
  })
}

############################--------=------
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
    
    for (i in 1:max_plots) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        
        my_i <- i 
        plotname <- paste("plot1", my_i, sep="")
        
        output[[plotname]] <- renderPlot({
          
          freq = as.matrix((theta())[(match(rownames((lift())[((lift())[,my_i] > 1),]),rownames((theta())))),][,my_i])
          
          freq1 = as.matrix(freq[(order(freq[,1], decreasing=T)),])
          
          if (nrow(freq1) >= 40) {n = 40}
          else {n = nrow(freq1)}
          
          mat  = tdm()[match(row.names(freq1),row.names(tdm())),]
          
          # mat = as.matrix(mat[order(rowSums(as.matrix(mat)),decreasing=T),][1:n,])
          # 
          # cmat  =  mat %*% t(mat)
          # diag(cmat) = 0
          # cmat[cmat <  quantile(cmat,.90)] = 0
          
          distill.cog(t(mat),'',
                      input$nodes,
                      input$connection)
          
          
          # graph <- graph.adjacency(cmat, mode = "undirected",weighted=T)
          # # par(mai=c(0,0,0,0))   		#this specifies the size of the margins. the default settings leave too much free space on all sides (if no axes are printed)
          # plot(  graph,			#the graph to be plotted
          #        layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
          #        #main='Organizational network example',	#specifies the title
          #        #vertex.label.dist=0.5,			#puts the name labels slightly off the dots
          #        vertex.frame.color='blue', 		#the color of the border of the dots 
          #        vertex.label.color='black',		#the color of the name labels
          #        vertex.label.font=1,			#the font of the name labels
          #        vertex.size = .00001,
          #        # vertex.label=col.names,		#specifies the lables of the vertices. in this case the 'name' attribute is used
          #        vertex.label.cex=1.3			#specifies the size of the font of the labels. can also be made to vary
          #        # title( main = paste("Segemnt ",my_i))
          # )
          
          mtext(paste("Topic",my_i), side = 3, line = 2, cex=2)
          
          box(lty = '11', col = 'black')
          
        })
      })
    }
        
#######################################
  eta = function(mat, dtm) {
  mat1 = mat/mean(mat);  terms1 = rownames(mat1);
  eta.mat = matrix(0, 1, ncol(mat1))
  
  for (i in 1:nrow(dtm)){
    a11 = as.data.frame(matrix(dtm[i,]));  rownames(a11) = colnames(dtm)
    a12 = as.matrix(a11[(a11>0),]);  rownames(a12) = rownames(a11)[(a11>0)];	rownames(a12)[1:4]
    a13 = intersect(terms1, rownames(a12));		a13[1:15];	length(a13)
    a14a = match(a13, terms1); 		# positions of matching terms in mat1 matrix
    a14b = match(a13, rownames(a12))		
    a15 = mat1[a14a,]*matrix(rep(a12[a14b,], ncol(mat1)), ncol = ncol(mat1))
    eta.mat = rbind(eta.mat, apply(a15, 2, mean))	
    rm(a11, a12, a13, a14a, a14b, a15)
  }
  eta.mat = eta.mat[2:nrow(eta.mat), ] 	# remove top zeroes row
  row.names(eta.mat)=row.names(dtm)
  return(eta.mat)
  }

  twc = reactive ({
  twc = eta((lift()),(x2mat()))
  return(twc)
  })

# 
#   output$summary2 <- renderTable({
#   
#       if (is.null(input$file)) {
#         # User has not uploaded a file yet
#         return(NULL)
#       }
#       
#       else {
# tb = data.frame(twc())
# colnames(tb) = paste("Topic",1:ncol(tb))
# round(tb,3)
#       }
#     })
#     

  da1 = reactive({
    if (is.null(input$file)) {return(NULL)}
    {
      tb = twc()
      tb = tb/rowSums(tb)
      tb = tb*100
      tb = data.frame(as.numeric(rownames(tb)),tb)
      colnames(tb) = c("Doc.id",paste("Topic",1:(ncol(tb)-1)))
      
      test = merge(tb,dataset(),by.x ="Doc.id", by.y= "Doc.id", all=T)
      return(test)}
  })
# Show table:
    output$table <- renderDataTable({
    da1()
    }, options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
    
    output$tmp <- renderPrint({
      if (is.null(input$file)) {return(NULL)}
      else {
      out = list(tdm(),x2mat(),theta(),lift(),twc())
      cat("Calculation Completed")}
    })

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

