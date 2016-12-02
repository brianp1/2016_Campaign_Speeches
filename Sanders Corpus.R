library(tidyverse)
library(tidytext)
library(rvest)
library(stringr)

#Getting Data
#Function to retrieve documents and clean it up for Sanders

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

#2016 Sanders Presidential Campaign Speeches
Sanders1 <- get_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=118045", "Sanders", "1")
Sanders2 <- get_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=117194", "Sanders", "2")
Sanders3 <- get_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=117513", "Sanders", "3")
Sanders4 <- get_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=116694", "Sanders", "4")
Sanders5 <- get_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=117516", "Sanders", "5")
Sanders6 <- get_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=117511", "Sanders", "6")
Sanders7 <- get_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=111440", "Sanders", "7")
Sanders8 <- get_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=117514", "Sanders", "8")
Sanders9 <- get_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=117512", "Sanders", "9")
Sanders10 <- get_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=114496", "Sanders", "10")
Sanders11 <- get_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=114487", "Sanders", "11")
Sanders12 <- get_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=117517", "Sanders", "12")
Sanders13 <- get_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=114493", "Sanders", "13")
Sanders14 <- get_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=114491", "Sanders", "14")
Sanders15 <- get_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=114486", "Sanders", "15")
Sanders16 <- get_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=114488", "Sanders", "16")
Sanders17 <- get_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=114494", "Sanders", "17")

Sanders_Corpus <- bind_rows(Sanders1,  Sanders2, Sanders3, Sanders4, Sanders5, Sanders6, Sanders7, Sanders8, Sanders9, Sanders10, Sanders11, Sanders12, Sanders13, Sanders14, Sanders15, Sanders16, Sanders17)

write_feather(Sanders_Corpus, "corpus/Sanders_Corpus.feather")
