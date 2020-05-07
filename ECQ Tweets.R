library("twitteR")
library("tm")
library(tidyverse)
library(tidytext)
install.packages("stopwords")
library("stopwords")

#I want to see the sentiments on the Enhanced Community Quarantine in the Philippines during Covid19

###### TWITTER ######


#Get consumerKey and consumerSecret from Twitter
consumer_key <- 'Lwgxv2epgxxKXmofp9POk28Nf'
consumer_secret <- 'bg1rQeTzRHLnwtsDflDEDzxioTg5tc4Qqq3OofyU8KTvkMs4oH'
access_token <- '1510474681-43P0PJGtC0zkeALdIPg7xkiGU85YLtqYcvNvEdc'
access_secret <- 'SnvddczcMJk7aWezu9tXkyMPtv1MrqWUYNAuITXigxAQL'

#Select 1 after entering setup_twitter_oauth to authorize
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#I get 1000 tweets that use #ecq starting on March 16, 2020
ecq <- twitteR::searchTwitter('#ecq', n = 1000, since = '2020-03-16', retryOnRateLimit = 1e3)
e = twitteR::twListToDF(ecq)

###### CLEAN UP ######

e$text <- as.character(e$text)

#These are to clean weird and repeating unnecessary parts of tweets
e$text = gsub("&amp", "", e$text) 
e$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", e$text)
e$text = gsub("@\\w+", "", e$text)
e$text = gsub("[[:punct:]]", "", e$text)
e$text = gsub("[[:digit:]]", "", e$text)
e$text = gsub("http\\w+", "", e$text)
e$text = gsub("[ \t]{2,}", "", e$text)
e$text = gsub("^\\s+|\\s+$", "", e$text) 

#To remove non-ascii chracters
e$text <- gsub("[^\x20-\x7E]", "", e$text)

#Since this is probably in Tagalog, I get the tagalog stop words from stopwords-iso
tagalog <- stopwords("tl", source = "stopwords-iso") 
tagalog_stop <- paste(tagalog, collapse = "|")

#Tagalog stopwords are changed to a blank
stopword2 <- paste0("\\b", tagalog_stop, "\\b")
stopword3 <- paste(stopword2, collapse = "|")
e$text <- gsub(pattern = stopword3, replacement = " ", x = e$text, ignore.case = TRUE)

#I create a tribble with other useless words that may appear
custom_stop <- tribble(
  ~word, ~lexicon,
  "ecq", "CUSTOM",
  "covid","CUSTOM",
  "quaranti","CUSTOM",
  "mainemendoza","CUSTOM",
  "maines","CUSTOM",
  "lang","CUSTOM",
  "quarantine","CUSTOM",
  "gcq","CUSTOM",
  "freethenipple","CUSTOM",
  "ecq","CUSTOM",
  "enhanced","CUSTOM") #this is removed as we are searching for "Enhanced Community Quarantine"

##### FREQUENCIES #####

#Tokens are unnested and words are counted
ecq_token_freq <- e %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  anti_join(custom_stop) %>% 
  count(word, sort=TRUE)

#This creates a bar plot of frequently used words
freq_hist <- e %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE) %>%
  top_n(10)%>%
  mutate(word=reorder(word, n)) %>% #Mutate reorder based on frequency. and then im overwriting my variable
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()

#This prints out the bar plot. Unfortunately, it does not say much
print(freq_hist)

##### BING ANALYSIS #####

ecq_bing <- e %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  anti_join(custom_stop) %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

ecq_bing %>%
  group_by(sentiment) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

ecq_bing %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=1000)

