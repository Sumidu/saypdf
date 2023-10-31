
library(pdftools)
library(tokenizers)
library(tidyverse)
source("say.R")


results <- pdftools::pdf_text(pdf = "chi24a-sub5949-i13.pdf")


# read in all the pages and separate all the lines
pagecount <- length(results)
dta <- tibble()
for (i in 1:pagecount) {
  page <- results[i]
  lines <- str_split(page, "\n")
  temp <- data_frame(lines=lines[[1]], page = i)
  dta <- bind_rows(dta, temp)
}


dta <- dta %>%
 # mutate(lines = str_remove_all(lines, "[0-9]")) %>%
  mutate(lines = str_remove_all(lines, "(\\s\\[[0-9,\\s]+?\\])")) %>% #remove all [123,124]
  mutate(lines = str_remove_all(lines, "(^[0-9]*)")) %>% #remove all [123,124]
#  mutate(lines = str_remove_all(lines, "•")) %>%
  mutate(lines = str_squish(lines)) %>%
  filter(str_length(lines)>0)

sentences <- dta %>%
  pull(lines) %>%
  paste(collapse = " ") %>%
  tokenize_sentences() %>%
  unlist()


sentences[1:10] %>% paste(collapse = " ")

script_name <- "script.txt"

fileConn<-file(script_name)
writeLines(sentences[1:100], fileConn)
close(fileConn)
say(fileinput = script_name)

