read_feather("corpus/Clinton_Corpus.feather")

mystopwords2 <- data_frame(word = c("texas", "smith", "cooper", "tianna", "barbara", "freia", "ruline", "miami", "reid", "caroline", "smith", "netanyahu", "michael", "gordon", "gordy", "sharansky", "don't", "that's", "they're", "we're", "mcdowell", "steve", "sanders", "milwaukee", "maine", "jackson", "indiana", "iowa", "september", "dr", "al", "gabby", "jack", "ben", "vermont","hillary", "people", "clinton", "america", "american", "country", "cheers"))
mystopwords2 <- bind_rows(stop_words, mystopwords2)

clinton_word_cloud <- Clinton_Corpus %>%
  select(word) %>%
  filter(word != "applause") %>%
  anti_join(mystopwords2)%>%
  count(word) 
wordcloud(words = clinton_word_cloud$word, freq = clinton_word_cloud$n, min.freq = 2, 
          max.words = 200, random.order = FALSE, rot.per = 0.15, 
          color=brewer.pal(6, "Dark2"))

Clinton_td <- Clinton_Corpus %>%
  group_by(docnumber) %>%
  filter(word != "applause") %>%
  count(word) %>%
  select(word, n, docnumber)
Clinton_td

Clinton_dtm <- Clinton_td %>%
  anti_join(mystopwords2) %>%
  cast_dtm(term = word,value = n, document = docnumber)
Clinton_dtm 

n_topics <- c(2, 3, 4, 5, 10, 15, 25, 50)
Clinton_comp <- n_topics %>%
  map(LDA, x = Clinton_dtm, control = list())
data_frame(k = n_topics,
           perplex = map_dbl(Clinton_comp, perplexity)) %>%
  ggplot(aes(k, perplex)) +
  geom_point() +
  geom_line()

clinton_lda <- LDA(Clinton_dtm, k = 15, control = list())
clinton_lda

clinton_lda_td <- tidy(clinton_lda)

clinton_terms <- clinton_lda_td %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
clinton_terms

perplexity(clinton_lda)

clinton_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic)))+
  geom_bar(stat= "identity", show.legend = FALSE)+
  facet_wrap(~topic, scales = "free")+
  coord_flip()
