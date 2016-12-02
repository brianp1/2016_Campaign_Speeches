library(tidyverse)
library(tidytext)
library(rvest)
library(stringr)

#Getting Data
#Function to retrieve documents and clean it up for Clinton

get_Clinton_speeches <- function(x, y){
  df1 <- read_html(x) %>%
    html_nodes("p") %>%
    html_text()
  
  df2 <- read_html(x)%>%
    html_node(".docdate")%>%
    html_text()
     
  
  speech <- data_frame(text = df1) %>%
    mutate(author = "Clinton",
           docnumber = y,
           parnumber = row_number(),
           date = df2) %>%
    separate(date, into = c("date2", "year"), sep = ",") %>%
    separate(date2, into = c("month", "day"), sep = " ")
  speech <- unnest_tokens(speech, word, text, token = "words")  
  return(speech)
}  
  
?spread

#2016 Clinton Presidential Campaign Speeches
#Clinton1 is Nov 3, 2016 - Clinton31 is Dec 15, 2015 
Clinton1 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119498", "1")
Clinton2 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119502", "2")
Clinton3 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119501", "3")
Clinton4 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119500", "4")
Clinton5 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119499", "5")
Clinton6 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119497", "6")
Clinton7 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119157", "7")
Clinton8 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119156", "8")
Clinton9 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119155", "9")
Clinton10 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119154", "10")
Clinton11 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119153", "11")
Clinton12 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119152", "12")
Clinton13 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119151", "13")
Clinton14 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119150", "14")
Clinton15 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119149", "15")
Clinton16 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119164", "16")
Clinton17 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119163", "17")
Clinton18 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119162", "18")
Clinton19 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119161", "19")
Clinton20 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119160", "20")
Clinton21 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119159", "21")
Clinton22 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119158", "22")
Clinton23 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119148", "23")
Clinton24 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=118051", "24")
Clinton25 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119295", "25")
Clinton26 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=116600", "26")
Clinton27 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=111596", "27")
Clinton28 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=111439", "28")
Clinton29 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=111414", "29")
Clinton30 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=111415", "30")
Clinton31 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119292", "31")
Clinton32 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=111418", "32")
Clinton33 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=111419", "33")
Clinton34 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=110267", "34")
Clinton35 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=110269", "35")
Clinton36 <- get_Clinton_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=110268", "36")

Clinton_Corpus <- bind_rows(Clinton1, Clinton2, Clinton3, Clinton4, Clinton5, Clinton6, Clinton7, Clinton8, Clinton9, Clinton10, Clinton11, Clinton12, Clinton13, Clinton14, Clinton15, Clinton16, Clinton17, Clinton18, Clinton19, Clinton20, Clinton21, Clinton22, Clinton23, Clinton24, Clinton25, Clinton26, Clinton27, Clinton28, Clinton29, Clinton30, Clinton31, Clinton32, Clinton33, Clinton34, Clinton35, Clinton36)

write_feather(Clinton_Corpus, "corpus/Clinton_Corpus.feather")
