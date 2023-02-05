library(tidyverse)
#install.packages("tidytext")

setwd("C:/Users/bdill/Downloads/")

df <- as.data.frame(read_lines("jan_6_report.txt"))
names(df) <- "lines"

# tokenize to one word per row
tokens <- df |> 
  tidytext::unnest_tokens(word, lines)    # by word, colname = "lines"

exclude_words <- c("a", "an", "and", "at", "by", "https", "in", "for","of", "on", "or", "the", "that", "to", "was", "with")

tokens2 <- tokens |> filter(!word %in% exclude_words)

count(tokens2, word) |> 
  filter(n > 100) |> 
  arrange(-n) |> 
  write_csv("jan6report_word_frequency.csv")

# saved to: https://docs.google.com/spreadsheets/d/15rNpTZgQ2u6SGhhfxrs_chfnloPSLxri0pmSG16x9Nk/edit#gid=1374437005

#----- Read Multiple PDFs -----
# https://data.library.virginia.edu/reading-pdf-files-into-r-for-text-mining/

#install.packages("tm")
install.packages("SnowballC")
library(tm)
library(pdftools)


setwd("C:/Users/bdill/Downloads/opinions")

files <- list.files(pattern = "pdf$")

corp <- Corpus(URISource(files),
               readerControl = list(reader = readPDF))

corp <- tm_map(corp, removePunctuation, ucp = TRUE)

opinions.tdm <- TermDocumentMatrix(corp, 
                                   control = 
                                     list(stopwords = TRUE,
                                          tolower = TRUE,
                                          stemming = TRUE,
                                          removeNumbers = TRUE,
                                          bounds = list(global = c(1, Inf)))) 
inspect(opinions.tdm[1:10,]) 

findFreqTerms(opinions.tdm, lowfreq = 100, highfreq = Inf)

ft <- findFreqTerms(opinions.tdm, lowfreq = 100, highfreq = Inf)
as.matrix(opinions.tdm[ft,]) 

ft.tdm <- as.matrix(opinions.tdm[ft,])
sort(apply(ft.tdm, 1, sum), decreasing = TRUE)

