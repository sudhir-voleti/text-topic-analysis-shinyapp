# Install / Load relevant packages
if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
pacman::p_load(shiny,text2vec, tm, tokenizers, wordcloud,slam,maptpx,igraph,DT,tools,stringr)
#pacman::p_load(DT)
