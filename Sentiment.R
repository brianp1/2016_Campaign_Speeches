read_feather("corpus/Trump_Corpus.feather")
read_feather("corpus/Clinton_Corpus.feather")
read_feather("corpus/Sanders_Corpus.feather")
read_feather("corpus/Repub_Corpus.feather")


speech_corpus <- bind_rows(Trump_Corpus, Clinton_Corpus, Sanders_Corpus, Repub_Corpus)


#Inserting Sentiment
speech_corpus_bing <- speech_corpus %>%
  inner_join(get_sentiments("bing"))
#83936 - 10608 = number of words discarded
speech_corpus_affin <- speech_corpus %>%
  inner_join(get_sentiments("afinn"))
#83936 - 11562 = number of words we discarded 
speech_corpus_nrc <- speech_corpus %>%
  inner_join(get_sentiments("nrc"))
#83936 - 43920 = number of words we discarded

speech_corpus_bing %>%
  count(author, sentiment) %>%
  mutate(percent= n/sum(n))%>%
  print()%>%
  ggplot(aes(author, percent, fill= author)) +
  geom_bar(alpha = .75, stat = "identity", width = .5 )+
  facet_grid(~sentiment)+
  labs(x= "Speaker",
       y= "Percentage of Text") +
  coord_flip()

speech_corpus_nrc %>%
  count(author, sentiment) %>%
  mutate(percent = n/sum(n)) %>%
  print()%>%
  ggplot(aes(sentiment, percent, fill= sentiment)) +
  geom_bar(alpha = .8, stat = "identity")+
  facet_wrap(~author) +
  coord_flip()

 
speech_corpus_nrc %>%
  count(author, sentiment) %>%
  mutate(percent= n/sum(n)) %>%
  ggplot(aes(sentiment, percent)) +
  geom_freqpoly(aes(color= author, group= author), stat = "identity")


speech_corpus_nrc %>%
  count(sentiment, author) %>%
  group_by(sentiment) %>%
  mutate(percent = n/sum(n)) %>%
  print() %>%
  ggplot(aes(author, percent, fill= author)) +
  geom_bar(stat = "identity", alpha = .85, show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +
  coord_flip() +
  labs(x = "sentiment",
       y= "% of Sentiment Between Candidates")


speech_corpus_affin %>%
  group_by(author, docnumber) %>%
  mutate(numeric_sentiment = cumsum(score)) %>%
  mutate(percent = as.integer(docnumber)) %>%
  mutate(percent= cumsum(percent/sum(percent))) %>%
  ggplot(aes(percent, numeric_sentiment, color = docnumber))+
  geom_freqpoly(stat = "identity") +
  facet_wrap(~author) %>%
  geom_line(aes(y = 75), size =2.25, linetype = 2)


#Important for producing a readable table with the results for the final project
#This provide tangible 
speech_corpus_affin %>%
  group_by(author, docnumber) %>%
  summarize(sum(score))
  
speech_corpus_affin %>%
  group_by(author, docnumber, month, year) %>%
  summarize(sum(score)) %>%
  group_by(author, month, year) %>%
  mutate(sum_sent = )

