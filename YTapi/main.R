library(textclean)
library(tm)
library(mgsub)
library(stringr)
library(ggplot2)
library(wordcloud)
library(tidytext)
library(dplyr)
library(tidyr)
library(ggthemes)
library(igraph)
library(ggraph)
library(widyr)
library(qdap)
library(caret)
library(yardstick)
library(tuber)
library(stringr)
library(stringi)

comments = read.csv("comments1March.csv", sep = ',')
comments = comments[,-1]

# scrap the latest version of emojis
readLines("https://www.unicode.org/Public/emoji/latest/emoji-test.txt",
          encoding="UTF-8") %>%
  stri_subset_regex(pattern = "^[^#]") %>%
  stri_subset_regex(pattern = ".+") -> emoji

# extract the emoji character
emoji %>%
  stri_extract_all_regex(pattern = "# *.{1,2} *") %>%
  stri_replace_all_fixed(pattern = c("*", "#"),
                         replacement = "",
                         vectorize_all=FALSE) %>%
  stri_trim_both() -> emoji.chars


# extract the emoji description
emoji %>%
  stri_extract_all_regex(pattern = "#.*$") %>%
  stri_replace_all_regex(pattern = "^#.*?E\\d+\\.\\d+\\s+",
                         replacement = " ") -> emoji.descriptions

commentsReplacedEmojis = stri_replace_all_regex(comments$Comment, #replace with yours case
                              pattern = emoji.chars,
                              replacement = emoji.descriptions,
                              vectorize_all=FALSE)
commentsErasedEmojis = stri_replace_all_regex(comments$Comment, #replace with yours case
                                              pattern = emoji.chars,
                                              replacement = " ",
                                              vectorize_all=FALSE)

comments$ReplacedEmoji = commentsReplacedEmojis
comments$ErasedEmoji = commentsErasedEmojis

# create corpus
corpus <- Corpus(VectorSource(comments$ReplacedEmoji))

corpus <- tm_map(corpus, content_transformer(tolower)) #all lowercase
corpus <- tm_map(corpus, removeNumbers) #remove numbers
corpus <- tm_map(corpus, removeWords,c(tm::stopwords('en'),'apple','face')) #remove english stopwords 
corpus <- tm_map(corpus, content_transformer(str_replace_all), '-', ' ') #replace dashes with spaces
corpus <- tm_map(corpus, content_transformer(str_replace_all), '–', ' ')
corpus <- tm_map(corpus, removePunctuation) #remove punctuation
corpus <- tm_map(corpus, stripWhitespace) #remove white spaces

comments$Comment[40]
content(corpus[[40]])

tdm <- TermDocumentMatrix(corpus) 
m <- as.matrix(tdm) #matrix
df <- data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE) #data.frame
v <- sort(rowSums(m),decreasing=TRUE) #term frequencies
d <- data.frame(word = names(v),freq=v)
dim(m)
nrow(m)
# wordcloud
library(wordcloud)
wordcloud(words = d$word, freq = d$freq, min.freq = 1000,
          max.words = 150, random.order = F, rot.per = 0.35,
          colors = brewer.pal(8, 'Dark2'))

#most frequent words
barplot(d[1:15,]$freq, las = 2, names.arg = d[1:15,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

#tokenization of cleaned corpus
wordTokens <- df %>% 
  unnest_tokens(word, text, drop = FALSE) %>% 
  select(-text) %>% 
  filter(!str_detect(word, '\\d')) %>% # delete words with numbers 
  filter(nchar(word) > 1) # delete words shorter than two letters

load(file = 'sentiments_lexicons.Rdata') #load lexicons

#apply sentiment polarity to words
wordSentiment <- wordTokens %>% 
  inner_join(bing, by = c('word' = 'word')) %>% 
  count(word, sentiment, sort = T) 

#aggregate them
word_sent_top <- wordSentiment %>% 
  group_by(sentiment) %>% 
  top_n(15, n) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) 

#plot them, most frequent words by sentiment in BING
ggplot(word_sent_top, aes(x = word, y = n, fill = sentiment)) +
  geom_col(show.legend = F) +
  facet_grid(~sentiment, scales = 'free_x') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

data_sent <- wordTokens %>% 
  inner_join(bing) %>% 
  count(sentiment, sort = T) %>% 
  spread(sentiment, n) %>% 
  mutate(diff = positive - negative) 
scoreSentiment <- data.frame(t(data_sent))
colnames(scoreSentiment) <- 'value'
t(scoreSentiment)

nrc_polarity <- nrc %>% 
  filter(sentiment %in% c('positive', 'negative'))

word_sent2 <- wordTokens %>% 
  inner_join(nrc_polarity, by = c('word' = 'word')) %>% 
  count(word, sentiment, sort = T) 

word_sent_top <- word_sent2 %>% 
  group_by(sentiment) %>% 
  top_n(15, n) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) 
###most frequent words by sentiment in NRC
ggplot(word_sent_top, aes(x = word, y = n, fill = sentiment)) +
  geom_col(show.legend = F) +
  facet_grid(~sentiment, scales = 'free_x') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

data_sent3 <- wordTokens %>% 
  inner_join(nrc) %>% 
  count(sentiment, sort = T) %>% 
  spread(sentiment, n)
scoreSentiment3 <- data.frame(t(data_sent3))
colnames(scoreSentiment3) <- c('value')
pl <- colnames(t(scoreSentiment3))
plot <- as.data.frame(cbind(pl, as.numeric(t(scoreSentiment3))))
colnames(plot) <- c('emotion', 'value')
plot$value <- as.numeric(plot$value)

ggplot(data=plot, aes(x=emotion, y=value)) +
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                                    axis.title.x =element_blank()) + geom_bar(stat="identity", fill=c('white','white',
                                                                                                      'white','white',
                                                                                                      'white','#ff7473',
                                                                                                      '#28bfc9','white',
                                                                                                      'white','white'), colour="black") + theme(legend.position = "none")
associations<-findAssocs(tdm, 'problem', 0.10)
associations<-as.data.frame(associations)
associations$terms<-row.names(associations)
associations$terms<-factor(associations$terms,
                           levels=associations$terms)

ggplot(associations, aes(y=terms)) +
  geom_point(aes(x=problem), data=associations,
             size=5)+
  theme_gdocs()+ geom_text(aes(x=problem,
                               label=problem),
                           colour="darkred",hjust=-.25,size=8)+
  theme(text=element_text(size=20),
        axis.title.y=element_blank())

associations<-findAssocs(tdm, 'great', 0.07)
associations<-as.data.frame(associations)
associations$terms<-row.names(associations)
associations$terms<-factor(associations$terms,
                           levels=associations$terms)

ggplot(associations, aes(y=terms)) +
  geom_point(aes(x=great), data=associations,
             size=5)+
  theme_gdocs()+ geom_text(aes(x=great,
                               label=great),
                           colour="darkred",hjust=0.5,size=4, vjust=-1)+
  theme(text=element_text(size=20),
        axis.title.y=element_blank())

# BI-GRAMS
corpusBi <- Corpus(VectorSource(comments$ErasedEmoji))

corpusBi <- tm_map(corpusBi, content_transformer(tolower)) #all lowercase
corpusBi <- tm_map(corpusBi, removeNumbers) #remove numbers
corpusBi <- tm_map(corpusBi, removeWords, c(tm::stopwords('en'))) #remove english stopwords 
corpusBi <- tm_map(corpusBi, content_transformer(str_replace_all), '-', ' ') #replace dashes with spaces
corpusBi <- tm_map(corpusBi, content_transformer(str_replace_all), '–', ' ')
corpusBi <- tm_map(corpusBi, removePunctuation) #remove punctuation
corpusBi <- tm_map(corpusBi, stripWhitespace) #remove white spaces

dfBi <- data.frame(text = sapply(corpusBi, as.character), stringsAsFactors = FALSE)

bigrams <- dfBi %>% 
  unnest_tokens(bigram, text, token = 'ngrams', n = 2)  %>% 
  separate(bigram, c('c1', 'c2'), sep = " " )

#most frequent bigrams
bigram_counts <- bigrams %>% 
  count(c1, c2, sort = TRUE) 


bigram_counts <- bigram_counts %>%
  drop_na(c1) %>%
  drop_na(c2)
head(bigram_counts, 10)

bigram_graph <- bigram_counts %>% 
  filter(n > 200) %>% 
  graph_from_data_frame() 


set.seed(1) 
#graph of bigrams
a <- grid::arrow(type = 'closed', length = unit(.1, 'inches')) # we want to have arrow on the plot

set.seed(1)
ggraph(bigram_graph, layout = "fr") + # to apply a layout to a graph
  geom_edge_link(aes(edge_alpha = n), # to make links more or less transparent, based on how frequent they appear
                 show.legend = FALSE,
                 arrow = a, # we will have arrows on the plot
                 end_cap = circle(.07, 'inches')) + # arrows will end before the touching the node
  geom_node_point(color = "red", size = 1) + 
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) + # to add names to the plot
  theme_void()

#see which words are preceded by a negation
negation_words <- c('headset', 'reality', 'immersion', 'society', 'children', 'buy') 
negated_words <- bigrams %>% 
  filter(c1 %in% negation_words) %>% 
  inner_join(afinn, by = c('c2' = 'word')) %>% 
  count(c1, c2, value, sort = TRUE) %>% 
  ungroup


top_neg_word <- negated_words %>% 
  mutate(contribution = n * value) %>% 
  arrange(desc(abs(contribution))) %>% 
  group_by(c1) %>% 
  top_n(10, abs(contribution)) %>% 
  ungroup() %>% 
  mutate(c2 = reorder(c2, contribution)) 
#plotting the frequences
ggplot(top_neg_word, aes(c2, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by negation") +
  ylab("Sentiment score * number of occurrences") +
  facet_wrap(~c1, ncol = 2, scales = 'free') + 
  coord_flip()

# ### **2.7 Correlation** 
# library(widyr)
#data without emojis

corpus2 <- Corpus(VectorSource(comments$ErasedEmoji))
# 
corpus2 <- tm_map(corpus2, content_transformer(tolower)) #all lowercase
corpus2 <- tm_map(corpus2, removeNumbers) #remove numbers
corpus2 <- tm_map(corpus2, removeWords, c(tm::stopwords('en'))) #remove english stopwords 
corpus2 <- tm_map(corpus2, content_transformer(str_replace_all), '-', ' ') #replace dashes with spaces
corpus2 <- tm_map(corpus2, content_transformer(str_replace_all), '–', ' ')
corpus2 <- tm_map(corpus2, removePunctuation) #remove punctuation
corpus2 <- tm_map(corpus2, stripWhitespace) #remove white spaces
# 
df2 <- data.frame(text = sapply(corpus2, as.character), stringsAsFactors = FALSE)
# # 

df2$review <- rep(c(1:34591),each = 3)
t = df2 %>% unnest_tokens(word, text, drop = FALSE) %>% 
select(-text) %>%
group_by(word) %>%
filter(n() >= 20) %>%
pairwise_cor(word, review, sort = TRUE) 
# 
# 
# 
# 
# 
# # We can pick some interesting words and find other most associated with them:
word_cors_top6 <- t %>%
  filter(item1 %in% c("good", "cool", "like", "better")) %>% # we choose those words
group_by(item1) %>% 
top_n(10) %>% # we choose top 10 (frequent) words 
ungroup() %>%
mutate(item2 = reorder(item2, correlation)) # new variable, that arranges second word acording to correlation
# 
# # graph With colors:
ggplot(word_cors_top6, aes(item2, correlation, fill = correlation)) + 
geom_col() + 
facet_wrap(~ item1, scales = "free") + # to organize plots
coord_flip() # flips orientation
# # graph With colors:
ggplot(word_cors_top6, aes(item2, correlation, fill = correlation)) + 
geom_col() + 
facet_wrap(~ item1, scales = "free") + # to organize plots
coord_flip() # flips orientation
# 
# 
# # We can now visualize the correlations and clusters of words:
# set.seed(2016)
# 
# # Let's use correlation matrix for network analysis:
t %>% filter(correlation > .50) %>% # filters data 
 graph_from_data_frame() %>%
 ggraph(layout = "fr") +
 geom_edge_link(show.legend = FALSE) +
 geom_node_point(color = "#277BC0", size = 4) +
 geom_node_text(aes(label = name), repel = TRUE) +
 theme_void()
# 
# # You can see that the output is rather symmetrical and there are no arrows - because
# # the relationship isn't directional. 
# # common (like in the bigram analysis earlier)


# TOPIC 
# Assuming you've already created 'tdm' as shown in your script

# Load the topicmodels package
library(topicmodels)

# Fit the LDA model
num_topics <- 5 # You might need to experiment with this number
lda_model <- LDA(tdm, k = num_topics, control = list(seed = 1234))

# Examine the top terms in each topic
top_terms <- terms(lda_model, 10) # Get top 10 terms for each topic
print(top_terms)

# Topic composition of documents
doc_topics <- posterior(lda_model)$topics
# This gives you a matrix where each row corresponds to a document and each column to a topic
# The values represent the proportion of words in the document that are attributed to each topic

# Optionally, visualize the topics
# Simple visualization of top terms per topic with ggplot2
library(ggplot2)
library(tidyr)

top_terms_df <- as.data.frame(top_terms)
top_terms_df <- pivot_longer(top_terms_df, cols = everything(), names_to = "Topic", values_to = "Term")
ggplot(top_terms_df, aes(x = reorder(Term, -as.numeric(Topic)), y = as.numeric(Topic))) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  labs(y = "Topic", x = "Term Frequency")


# TOPIC 2
## Topic Modeling



K <- 20

topicModel <- LDA(DTM.trim, 
                  K, 
                  method = "Gibbs", 
                  control = list(iter = 500, 
                                 verbose = 25,
                                 seed = 1234))

tmResult <- posterior(topicModel)



beta <- tmResult$terms
glimpse(beta)



theta <- tmResult$topics
glimpse(theta)

terms(topicModel, 10)

top5termsPerTopic <- terms(topicModel, 5)


topicNames <- apply(top5termsPerTopic, 
                    2, 
                    paste, 
                    collapse = " ")


topicProportions <- colSums(theta) / nrow(DTM)  # average probability over all paragraphs
names(topicProportions) <- topicNames     # Topic Names
sort(topicProportions, decreasing = TRUE) # sort



