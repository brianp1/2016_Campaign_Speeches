read_feather("corpus/Repub_Corpus.feather")

mystopwords2 <- data_frame(word = c("texas", "smith", "cooper", "tianna", "barbara", "freia", "ruline", "miami", "reid", "caroline", "smith", "netanyahu", "michael", "gordon", "gordy", "sharansky", "don't", "that's", "they're", "we're", "mcdowell", "steve", "milwaukee", "maine", "jackson", "indiana", "iowa", "september", "dr", "al", "gabby", "jack", "ben", "vermont", "people", "cheers"))
mystopwords2 <- bind_rows(stop_words, mystopwords2)

repub_word_cloud <- Repub_Corpus %>%
  select(word) %>%
  filter(word != "applause") %>%
  anti_join(mystopwords2)%>%
  count(word) 
wordcloud(words = repub_word_cloud$word, freq = repub_word_cloud$n, min.freq = 2, 
          max.words = 200, random.order = FALSE, rot.per = 0.35, 
          color=brewer.pal(6, "Dark2"))

Repub_td <- Repub_Corpus %>%
  group_by(docnumber) %>%
  filter(word != "applause") %>%
  count(word) %>%
  select(word, n, docnumber)
Repub_td

Repub_dtm <- Repub_td %>%
  anti_join(mystopwords2) %>%
  cast_dtm(term = word,value = n, document = docnumber)
Repub_dtm 

n_topics <- c(2, 3, 4, 5, 10, 15, 25, 50)
Repub_comp <- n_topics %>%
  map(LDA, x = Repub_dtm, control = list())
data_frame(k = n_topics,
           perplex = map_dbl(Repub_comp, perplexity)) %>%
  ggplot(aes(k, perplex)) +
  geom_point() +
  geom_line()

repub_lda <- LDA(Repub_dtm, k = 10, control = list())
repub_lda

repub_lda_td <- tidy(repub_lda)

repub_terms <- repub_lda_td %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
repub_terms

perplexity(repub_lda)

repub_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic)))+
  geom_bar(stat= "identity", show.legend = FALSE)+
  facet_wrap(~topic, scales = "free")+
  coord_flip()
