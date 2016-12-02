library(tidyverse)
library(tidytext)
library(rvest)
library(stringr)

#Getting Data
#Function to retrieve documents and clean it up for Cruz

get_speeches <- function(x, y, z){
  df1 <- read_html(x) %>%
    html_nodes("p") %>%
    html_text()
  
  df2 <- read_html(x)%>%
    html_node(".docdate")%>%
    html_text()
  
  speech <- data_frame(text = df1) %>%
    mutate(author = y,
           docnumber = z,
           parnumber = row_number(),
           date = df2) %>%
    separate(date, into = c("date2", "year"), sep = ",") %>%
    separate(date2, into = c("month", "day"), sep = " ")
  speech <- unnest_tokens(speech, word, text, token = "words")  
  return(speech)
}


Cruz1 <- get_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=118041", "Cruz", "1")
Cruz2 <- get_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=117232", "Cruz", "2")
Cruz3 <- get_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=116598", "Cruz", "3")
Cruz4 <- get_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=114768", "Cruz", "4")
Rubio1 <- get_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=111441", "Rubio", "5")
Kasich1 <- get_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=116599", "Kasich", "6")
Kasich2 <- get_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=113069", "Kasich", "7")

Repub_Corpus <- bind_rows(Cruz1, Cruz2, Cruz3, Cruz4, Rubio1, Kasich1, Kasich2)

write_feather(Repub_Corpus, "corpus/Repub_Corpus.feather")
