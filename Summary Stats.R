read_feather("corpus/Trump_Corpus.feather")
read_feather("corpus/Clinton_Corpus.feather")
read_feather("corpus/Sanders_Corpus.feather")
read_feather("corpus/Repub_Corpus.feather")


speech_corpus <- bind_rows(Trump_Corpus, Clinton_Corpus, Sanders_Corpus, Repub_Corpus)


speech_corpus %>%
  group_by(author) %>%
  count(word) %>%
  summarise(total = sum(n))

speech_corpus %>%
  group_by(author) %>%
  filter(word == "applause") %>%
  count() %>%
  kable()
# Which one is closer to finding the mean?

speech_corpus %>%
  group_by(author, docnumber) %>%
  filter(word == "applause") %>%
  count() %>%
  mutate(app_sum = sum(n))  %>%
  mean(n)

speech_corpus %>%
  group_by(author) %>%
  filter(word == "applause") %>%
  count() %>%
  mean(n)

