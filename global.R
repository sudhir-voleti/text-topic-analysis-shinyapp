max_plots = 20
wc1 = function(n,mat1,mat2){
  for (i in 1:ncol(mat2)) {
  freq = as.matrix(mat1[(match(rownames(mat2[(mat2[,i] > 1),]),rownames(mat1))),][,i])
    freq = as.matrix(freq[(order(freq[,1], decreasing=T)),])
    top_word = as.matrix(freq[1:n,])
    #mypath <- (paste("WC_mat2_theta_topic",i,"_",n, ".jpeg", sep = ""))
    #jpeg(file = mypath, pointsize = 12,  width = 800, height = 800, quality=200)
    wordcloud(rownames(top_word), top_word, scale = c(8, 1), 1, random.order=FALSE, random.color=FALSE, colors=c(1:4));
    dev.off()
  }
}

text.clean = function(x)                    # text data
{ require("tm")
  x  =  gsub("<.*?>", " ", x)               # regex for removing HTML tags
  x  =  iconv(x, "latin1", "ASCII", sub="") # Keep only ASCII characters
  x  =  gsub("[^[:alnum:]]", " ", x)        # keep only alpha numeric
  x  =  tolower(x)                          # convert to lower case characters
  x  =  removeNumbers(x)                    # removing numbers
  x  =  stripWhitespace(x)                  # removing white space
  x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
  return(x)
}

dtm.word.count <- function(dtm) {
  
  if (ncol(dtm) > 1000) {
    tst = round(ncol(dtm)/100)  # divide DTM's cols into 100 manageble parts
    a = rep(tst,99)
    b = cumsum(a);rm(a)
    b = b[-which(b >= ncol(dtm))]
    b = c(0,b,ncol(dtm))
    
    ss.col = c(NULL)
    for (i in 1:(length(b)-1)) {
      tempdtm = dtm[,(b[i]+1):(b[i+1])]
      s = colSums(as.matrix(tempdtm))
      ss.col = c(ss.col,s)
    }
  } else {
    ss.col = colSums(as.matrix(dtm))
  }
  
  tsum = ss.col
  # tsum = tsum[order(tsum, decreasing = T)]       #terms in decreasing order of freq
  return(tsum)
}


dtm.word.cloud <- function(count = count, max.words = 100,title = "Title"){
  
  if (class(count)[1] == "DocumentTermMatrix"|class(count)[1] == "simple_triplet_matrix")
  {
    tsum = dtm.word.count(count)
  } else {
    tsum = count
  }
  
  if (class(tsum) != "numeric") stop("Give input as wordcount or DocumentTermMatrix")
  
  wordcloud(names(tsum), tsum,     # words, their freqs 
            scale = c(4, 0.5),     # range of word sizes
            # min.frq = min.frq,                     # min.freq of words to consider
            max.words = max.words,       # max #words
            colors = brewer.pal(8, "Dark2"))    # Plot results in a word cloud 
  title(sub = title)     # title for the wordcloud display
}   

distill.cog.tcm = function(mat1, # input TCM or DTM MAT
                           mattype = "DTM", # "DTM" or TCM
                           title, # title for the graph
                           s,    # no. of central nodes
                           k1){  # max no. of connections  
  require(igraph)
  
  mat1 = as.matrix(mat1)
  
  if (mattype == "DTM"){
    mat1 = tcrossprod(t(mat1))
  }
  
  if (mattype == "TCM"){
    mat1 = as.matrix(mat1)
    mat1 = mat1 + t(mat1)
  }
  
  
  if (ncol(mat1) > 1000) {
    tst = round(ncol(mat1)/100)  # divide mat1's cols into 100 manageble parts
    a = rep(tst,99)
    b = cumsum(a);rm(a)
    b = b[-which(b >= ncol(mat1))]
    b = c(0,b,ncol(mat1))
    
    ss.col = c(NULL)
    for (i in 1:(length(b)-1)) {
      tempmat1 = mat1[,(b[i]+1):(b[i+1])]
      su = colSums(as.matrix(tempmat1))
      ss.col = c(ss.col,su);rm(su)
    }
  } else {
    ss.col = colSums(as.matrix(mat1))
  }
  
  # a = colSums(mat1) # collect colsums into a vector obj a
  a = ss.col
  b = order(-a)     # nice syntax for ordering vector in decr order  
  
  mat2 = mat1[b, b]     # order both rows and columns along vector b
  
  diag(mat2) =  0
  
  ## +++ go row by row and find top k adjacencies +++ ##
  
  wc = NULL
  
  for (i1 in 1:s){ 
    thresh1 = mat2[i1,][order(-mat2[i1, ])[k1]]
    mat2[i1, mat2[i1,] < thresh1] = 0   # neat. didn't need 2 use () in the subset here.
    mat2[i1, mat2[i1,] > 0 ] = 1
    word = names(mat2[i1, mat2[i1,] > 0])
    mat2[(i1+1):nrow(mat2), match(word,colnames(mat2))] = 0
    wc = c(wc,word)
  } 
  
  mat3 = mat2[match(wc, colnames(mat2)), match(wc, colnames(mat2))]
  ord = colnames(mat2)[which(!is.na(match(colnames(mat2), colnames(mat3))))]  # removed any NAs from the list
  mat4 = mat3[match(ord, colnames(mat3)), match(ord, colnames(mat3))]
  
  graph <- graph.adjacency(mat4, mode = "undirected", weighted=T)    # Create Network object
  
  graph = simplify(graph) 
  V(graph)$color[1:s] = "green"
  V(graph)$color[(s+1):length(V(graph))] = "pink"
  
  graph = delete.vertices(graph, V(graph)[ degree(graph) == 0 ]) # delete singletons?
  
  plot(graph, 
       layout = layout.kamada.kawai, 
       main = title)
} 
dtm.tcm.creator <- function(text,id = "",
                            std.clean = TRUE,
                            std.stop.words = TRUE,
                            stop.words.additional ,
                            bigram.encoding = TRUE,
                            bigram.min.freq = 5,
                            min.dtm.freq = 2,
                            skip.grams.window = 5) {
  
  # if (class(text) != "character" | length(text) < 3){
  #   stop("data format Not correct. Make sure it's a character verctor of length above 3")
  # }
  
  stpw1 = readLines("data/stopwords.txt")# stopwords list from git
  stpw2 = tm::stopwords('english')      # tm package stop word list; tokenizer package has the same name function, hence 'tm::'
  stpw3  = unique(gsub("'"," ",c(stpw1,stpw2)))
  
  if ((id == "")[1]){
    id = 1:length(text)
  }
  
  if (std.clean == TRUE) {
    # print("Performing Standard Text Cleaning")
    text = text.clean(text)
  }
  
  if (std.stop.words == TRUE){
    # print("Removing Stop Words")
    stop.words.f = unique(c(stpw3,stop.words.additional))
    text = removeWords(text,stop.words.f)            # removing stopwords created above
    text = stripWhitespace(text)                  # removing white spacestop.words.additional
  }
  
  tok_fun = word_tokenizer  # using word & not space tokenizers
  
  if (bigram.encoding == TRUE){
    
    # data = data.frame(id = 1:length(text),text = text, stringsAsFactors = F)
    
    # print("finding bi-grams for encoding with selected criteria")
    
    it_0 = itoken( text,
                   tokenizer = tok_fun,
                   ids = id,
                   progressbar = F)
    
    vocab = create_vocabulary(it_0, ngram = c(2L, 2L))
    pruned_vocab = data.frame(prune_vocabulary(vocab, term_count_min = bigram.min.freq))
    
    replace_list = pruned_vocab$term[order(pruned_vocab$term_count, decreasing = T)]
    
    # Cut the bi-grams upto 200 words
    
    if (length(replace_list) > 200){
      replace_list = replace_list[1:200]
    }
    
    if (length(replace_list) > 1){    # my edit. was 0, now 1.
      text = paste("",text,"")
      
      pb <- txtProgressBar(min = 1, max = (length(replace_list)), style = 3) ; i = 0
      
      # print(paste("Encoding",length(replace_list),"bi-grams as unigram"))
      for (term in replace_list){
        i = i + 1
        focal.term = gsub("_", " ",term)        # in case dot was word-separator
        replacement.term = term
        text = gsub(paste("",focal.term,""),paste("",replacement.term,""), text)
        # setTxtProgressBar(pb, i)
      }                  
    } else {
      print("No bigram to encode with selected criteria")}
  }
  
  # print("Creating Document Term Matrix")
  # Create DTM
  it_m = itoken(text,
                tokenizer = tok_fun,
                ids = id,
                progressbar = F)
  
  vocab = create_vocabulary(it_m)
  pruned_vocab = prune_vocabulary(vocab,
                                  term_count_min = min.dtm.freq)
  
  vectorizer = vocab_vectorizer(pruned_vocab)
  
  dtm_m  = create_dtm(it_m, vectorizer)
  
  # print("Creating Term Co-occurrence Matrix")
  
  # vectorizer = vocab_vectorizer(pruned_vocab,
  #                               grow_dtm = FALSE,
  #                               skip_grams_window = skip.grams.window)
  # 
  # tcm = create_tcm(it_m, vectorizer) # func to build a TCM
  
  # print("Done!!")
  
  out = list(dtm = dtm_m)#, tcm = tcm)
  return(out)
}


fit.lda.topics  <- function(dtm,K=2 ){
  
  simfit <- topics(dtm,  K=K, verb=3)
  theta = simfit$theta
  omega = simfit$omega
  
  if (ncol(dtm) > 1000) {
    tst = round(ncol(dtm)/100)  # divide DTM's cols into 100 manageble parts
    a = rep(tst,99)
    b = cumsum(a);rm(a)
    b = b[-which(b >= ncol(dtm))]
    b = c(0,b,ncol(dtm))
    
    ss.col = c(NULL)
    for (i in 1:(length(b)-1)) {
      tempdtm = dtm[,(b[i]+1):(b[i+1])]
      s = colSums(as.matrix(tempdtm))
      ss.col = c(ss.col,s)
    }
  } else {
    ss.col = colSums(as.matrix(dtm))
  }
  
  lift = theta*0;       # lift will have same dimn as the theta matrix
  
  sum1 = sum(dtm)
  pterms = ss.col/sum1     # each column's marginal occurrence probability
  
  for (i in 1:nrow(theta)){  
    for (j in 1:ncol(theta)){
      ptermtopic = 0; pterm = 0;
      ptermtopic = theta[i, j]
      pterm = pterms[i]
      lift[i, j] = ptermtopic/pterm     # divide each cell by the column's marg. occurr. proby.
    }
  }
  
  censored.lift = lift
  
  for (i in 1:nrow(lift)){
    censored.lift[i,][censored.lift[i,] < max(censored.lift[i,])] = 0   # hard assigning tokens to topics
  }
  
  
  if(nrow(dtm) < 1000) {k1 = 2} else {k1= 100}   # k1=10 tha, ab k1=2 kar diya. to avoid machine choking up in v small datasets
  
  tst = ceiling(nrow(dtm)/k1)  # now using 1% of the rows at a time
  a = rep(tst, (k1 - 1))
  b = cumsum(a);rm(a)    # cumsum() is cumulative sum.
  b = c(0, b, nrow(dtm))  # broke the supermassive dtm into chunks of 1% ncol each
  a0 = which(b > nrow(dtm));    # sometimes, rounding errors cause out of bound errors
  if (length(a0) > 0) {b = b[-a0]}
  
  eta.new = NULL
  for (i1 in 1:K){
    
    a2 = c(NULL)
    for (i in 1:(length(b)-1)) {
      tempdtm = dtm[(b[i]+1):(b[i+1]),]
      a = matrix(rep(lift[, i1], nrow(tempdtm)), nrow(tempdtm), ncol(tempdtm), byrow = TRUE)
      a1 = rowSums(as.matrix(tempdtm * a))
      a2 = c(a2, a1); rm(a, a1, tempdtm)
    } # i ends
    
    eta.new = cbind(eta.new, a2); rm(a2)
  } # i1 ends
  
  # rownames(eta.new) = rownames(simfit$omega)
  colnames(eta.new) = colnames(simfit$theta)
  
  kappa = eta.new / rowSums(eta.new)   # calc topic proportions for each document
  output = list(simfit = simfit, theta = theta,omega = omega, lift = lift, censored.lift = censored.lift,
                eta = eta.new, kappa = kappa )
  return(output)
}
