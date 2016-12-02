library(tidyverse)
library(tidytext)
library(rvest)
library(stringr)
library(feather)
library(topicmodels)
library(knitr)

get_Trump_speeches <- function(x, y){
  df1 <- read_html(x) %>%
    html_nodes("p") %>%
    html_text()
  
  df2 <- read_html(x)%>%
    html_node(".docdate")%>%
    html_text()
  
  
  speech <- data_frame(text = df1) %>%
    mutate(author = "Trump",
           docnumber = y,
           parnumber = row_number(),
           date = df2) %>%
    separate(date, into = c("date2", "year"), sep = ",") %>%
    separate(date2, into = c("month", "day"), sep = " ")
  speech <- unnest_tokens(speech, word, text, token = "words")  
  return(speech)
}  

Trump1 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119182", "1")
Trump2 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119181", "2")
Trump3 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119188", "3")
Trump4 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119187", "4")
Trump5 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119186", "5")
Trump6 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119185", "6")
Trump7 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119184", "7")
Trump8 <- get_Trump_speeches('http://www.presidency.ucsb.edu/ws/index.php?pid=119183', "8")
Trump9 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119174", "9")
Trump10 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119172", "10")
Trump11 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119180", "11")
Trump12 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119173", "12")
Trump13 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119170", "13")
Trump14 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119169", "14")
Trump15 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119168", "15")
Trump16 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119167", "16")
Trump17 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119166", "17")
Trump18 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119179", "18")
Trump19 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119202", "19")
Trump20 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119201", "20")
Trump21 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119200", "21")
Trump22 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119203", "22")
Trump23 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119191", "23")
Trump24 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119189", "24")
Trump25 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119192", "25")
Trump26 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119207", "26")
Trump27 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119208", "27")
Trump28 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119209", "28")
Trump29 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119190", "29")
Trump30 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119206", "30")
Trump31 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119206", "31")
Trump32 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119193", "32")
Trump33 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119205", "33")
Trump34 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119178", "34")
Trump35 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119204", "35")
Trump36 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119194", "36")
Trump37 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119195", "37")
Trump38 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119177", "38")
Trump39 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119197", "39")
Trump40 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119199", "40")
Trump41 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119198", "41")
Trump42 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119196", "42")
Trump43 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119176", "43")
Trump44 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119175", "44")
Trump45 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119165", "45")
Trump46 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=119503", "46")
Trump47 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=117935", "47")
Trump48 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=117791", "48")
Trump49 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=117815", "49")
Trump50 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=117790", "50")
Trump51 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=117775", "51")
Trump52 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=117813", "52")
Trump53 <- get_Trump_speeches("http://www.presidency.ucsb.edu/ws/index.php?pid=116597", "53")

Trump_Corpus <- bind_rows(Trump1, Trump2, Trump3, Trump4, Trump5, Trump6, Trump7, Trump8, Trump9, Trump10, Trump11, Trump12, Trump13, Trump14, Trump15, Trump16, Trump17, Trump18, Trump19, Trump20, Trump21, Trump22, Trump23, Trump24, Trump25, Trump26, Trump27, Trump28, Trump29, Trump30, Trump31, Trump32, Trump33, Trump34, Trump35, Trump36, Trump37, Trump38, Trump39, Trump40, Trump41, Trump42, Trump42, Trump43, Trump44, Trump45, Trump46, Trump47, Trump48, Trump49, Trump50, Trump51, Trump52, Trump53)

write_feather(Trump_Corpus, "corpus/Trump_Corpus.feather")


