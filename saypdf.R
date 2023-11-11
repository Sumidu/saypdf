# Load libraries and functions
library(pdftools)
library(tokenizers)
library(tidyverse)
source("say.R")


results <- pdftools::pdf_text(pdf = "chi24a-sub5949-i13.pdf")


# read in all the pages and separate all the lines
pagecount <- length(results)
rawlines <- tibble()
for (i in 1:pagecount) {
  page <- results[i]
  line <- str_split(page, "\n")
  temp <- tibble(line=line[[1]], page = i)
  rawlines <- bind_rows(rawlines, temp)
}


clean_lines <- rawlines %>%
  mutate(line = str_remove_all(line, "(\\s\\[[0-9,\\s]+?\\])")) %>% #remove all [123,124]
  mutate(line = str_remove_all(line, "(^[0-9]*)")) %>% #remove rowlines
#  mutate(lines = str_remove_all(lines, "•")) %>%
  mutate(line = str_squish(line)) %>%
  filter(length(line)>0)


# find chapters
chapter_indices <- which(str_detect(clean_lines$line, "^[0-9] [:upper:]+?")) # 1 Chapter
section_indices <- which(str_detect(clean_lines$line, "^[0-9]\\.[0-9] .+?")) # 1.1 Subsection
subsection_indices <- which(str_detect(clean_lines$line, "^[0-9]\\.[0-9]\\.[0-9] .+?")) # 1.1.1 Subsubsection. More text...

ref_section_index <- which(str_detect(clean_lines$line, "REFERENCES"))
page_numbers <- which(str_detect(clean_lines$line, "^[0-9]+$")) # Lines that only have a single number

# are these correct?
clean_lines$line[chapter_indices]
clean_lines$line[section_indices]
clean_lines$line[subsection_indices]
clean_lines$line[ref_section_index]
clean_lines$line[page_numbers]


# find headers
heading_lookahead_max <- 10
even_heading <- ""
odd_heading <- ""
for (i in 1:heading_lookahead_max){
  line <- clean_lines[page_numbers[1]+i,]$line
  if (str_length(line)>0){
    even_heading <- line
    break
  }
}
message(paste("Even heading:", even_heading, "\n"))

for (i in 1:heading_lookahead_max){
  line <- clean_lines[page_numbers[2]+i,]$line
  if (str_length(line)>0){
    odd_heading <- line
    break
  }
}
message(paste("Odd heading:", odd_heading, "\n"))


# text without page numbers, etc
clean_lines[c(-page_numbers,-chapter_indices,-section_indices),]

removed_meta_lines <- clean_lines %>%
  slice(c(-page_numbers,-ref_section_index:-n())) %>%
  filter(line != even_heading) %>% # remove headings
  filter(line != odd_heading)


# find chapters again (after slicing)
chapter_indices <- which(str_detect(removed_meta_lines$line, "^[0-9] [:upper:]+?")) # 1 Chapter
section_indices <- which(str_detect(removed_meta_lines$line, "^[0-9]\\.[0-9] .+?")) # 1.1 Subsection
subsection_indices <- which(str_detect(removed_meta_lines$line, "^[0-9]\\.[0-9]\\.[0-9] .+?")) # 1.1.1 Subsubsection. More text...




sentences <- removed_meta_lines %>%
  mutate(line = ifelse(row_number() %in% chapter_indices, paste0(line,"."), line)) %>%
  mutate(line = ifelse(row_number() %in% section_indices, paste0(line,"."), line)) %>%
  pull(line) %>%
  paste(collapse = " ") %>%
  tokenize_sentences() %>%
  unlist()



script_name <- "script.txt"

fileConn<-file(script_name)
writeLines(sentences, fileConn)
close(fileConn)

#say
say(fileinput = script_name, filename = "audio.aiff", overwrite = TRUE)

