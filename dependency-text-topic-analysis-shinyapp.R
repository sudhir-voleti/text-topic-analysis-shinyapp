# Install / Load relevant packages
if (!require(shiny)){install.packages("shiny"); library(shiny)}
if (!require(pdftools)){install.packages("pdftools", quiet = TRUE)}; library(pdftools)
if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
pacman::p_load(shiny,text2vec, tm, tokenizers, wordcloud,slam,maptpx,igraph,DT,tools,stringr,dplyr)
#pacman::p_load(DT)
