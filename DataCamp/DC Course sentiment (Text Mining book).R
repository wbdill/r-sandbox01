
# https://www.tidytextmining.com/tidytext.html

install.packages("tidyverse")
install.packages("tidytext")
install.packages("syuzhet")     # lexicons
install.packages("janeaustenr") # Jane Austin books

#----- lexicons ----------------------------------------------------------------
library(syuzhet)

# 4 lexicons: loughran, afinn, bing, nrc
get_sentiments("loughran") # pos/neg
get_sentiments("bing")     #pos/neg
get_sentiments("afinn")    # +/- score
get_sentiments("nrc")      #categories ex: trust, fear, anger

#------- Jane Austin book load -------------------------------------------------
rm(list = ls())
library(tidyverse)
library(janeaustenr)
library(dplyr)
library(stringr)
library(tidyr)



#----- Project Gutenburg -------------------------------------------------------
install.packages("gutenbergr")
library(gutenbergr)

# Top 100 books: https://www.gutenberg.org/browse/scores/top
#  Let's get The Time Machine, The War of the Worlds, The Invisible Man, 
#and The Island of Doctor Moreau.
hgwells <- gutenberg_download(c(35, 36, 5230, 159))

# War and Peace is 2600  https://www.gutenberg.org/ebooks/2600
warpeace <- gutenberg_download(c(2600))
write.csv(warpeace, "D:/data/R/war_and_peace.csv")

# Save an object to a file
saveRDS(warpeace, file = "D:/data/R/war_and_peace.rds")
# Restore the object
mydata <- readRDS(file = "D:/data/R/war_and_peace.rds")

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
