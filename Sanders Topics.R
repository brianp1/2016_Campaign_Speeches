read_feather("corpus/Sanders_Corpus.feather")

mystopwords2 <- data_frame(word = c("texas", "smith", "cooper", "tianna", "barbara", "freia", "ruline", "miami", "reid", "caroline", "smith", "netanyahu", "michael", "gordon", "gordy", "sharansky", "don't", "that's", "they're", "we're", "mcdowell", "steve", "sanders", "milwaukee", "maine", "jackson", "indiana", "iowa", "september", "dr", "al", "gabby", "jack", "ben", "vermont","hillary", "people", "clinton", "america", "american", "country", "cheers"))
mystopwords2 <- bind_rows(stop_words, mystopwords2)

sanders_word_cloud <- Sanders_Corpus %>%
  select(word) %>%
  filter(word != "applause") %>%
  anti_join(mystopwords2)%>%
  count(word) 
wordcloud(words = sanders_word_cloud$word, freq = sanders_word_cloud$n, min.freq = 2, 
          max.words = 200, random.order = FALSE, rot.per = 0.35, 
          color=brewer.pal(6, "Dark2"))

Sanders_td <- Sanders_Corpus %>%
  group_by(docnumber) %>%
  filter(word != "applause") %>%
  count(word) %>%
  select(word, n, docnumber)
Sanders_td

Sanders_dtm <- Sanders_td %>%
  anti_join(mystopwords2) %>%
  cast_dtm(term = word,value = n, document = docnumber)
Sanders_dtm 

n_topics <- c(2, 3, 4, 5, 10, 15, 25, 50)
Sanders_comp <- n_topics %>%
  map(LDA, x = Sanders_dtm, control = list())
data_frame(k = n_topics,
           perplex = map_dbl(Sanders_comp, perplexity)) %>%
  ggplot(aes(k, perplex)) +
  geom_point() +
  geom_line()

sanders_lda <- LDA(Sanders_dtm, k = 15, control = list())
sanders_lda

sanders_lda_td <- tidy(sanders_lda)

sanders_terms <- sanders_lda_td %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
sanders_terms

perplexity(sanders_lda)

sanders_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic)))+
  geom_bar(stat= "identity", show.legend = FALSE)+
  facet_wrap(~topic, scales = "free")+
  coord_flip()
