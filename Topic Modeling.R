read_feather("corpus/Trump_Corpus.feather")
read_feather("corpus/Clinton_Corpus.feather")
read_feather("corpus/Sanders_Corpus.feather")
read_feather("corpus/Repub_Corpus.feather")


speech_corpus <- bind_rows(Trump_Corpus, Clinton_Corpus, Sanders_Corpus, Repub_Corpus)


speech_td <- speech_corpus %>%
  group_by(author, docnumber) %>%
  filter(word != "applause")
  count(word) %>%
  select(author, word, n, docnumber) %>%
  mutate(docid = paste0(author, docnumber))
speech_td

mystopwords <- data_frame(word = c("texas", "smith", "cooper", "tianna", "barbara", "freia", "ruline", "miami", "reid", "caroline", "smith", "netanyahu", "michael", "gordon", "gordy", "sharansky", "don't", "that's", "they're", "we're", "mcdowell", "steve", "sanders", "milwaukee", "maine", "jackson", "indiana"))
mystopwords <- bind_rows(stop_words, mystopwords)

speech_dtm <- speech_td %>%
  anti_join(mystopwords) %>%
  cast_dtm(term = word,value = n, document = docid)
speech_dtm 

speech_dtm %>%

speech_lda <- LDA(speech_dtm, k = 4, control = list(seed = 1234))
speech_lda

speech_lda_td <- tidy(speech_lda)

top_terms <- speech_lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
View(top_terms)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic)))+
    geom_bar(stat= "identity", show.legend = FALSE)+
    facet_wrap(~topic, scales = "free_y")+
    coord_flip()


inverse_doc_freq <- speech_td %>%
  bind_tf_idf(word, docid, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))
inverse_doc_freq


ggplot(inverse_doc_freq[1:25,], aes(word, tf_idf, fill = author)) +
  geom_bar(alpha = 0.8, stat = "identity", scales = "free") +
  coord_flip()

inverse_doc_freq %>%
  group_by(author) %>%
  top_n(20)%>%
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = author)) +
  geom_bar(stat = "identity") +
  facet_wrap(~author, scales = "free") +
  coord_flip()


