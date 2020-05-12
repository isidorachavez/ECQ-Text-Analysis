#Loading libraries
library("readxl")
library(tidyverse)
library(tidytext)
library(dplyr)

#Reading Excel
cnn  <- read_excel('cnn.xlsx')
msn  <- read_excel('msn.xlsx')
star <- read_excel('star.xlsx')

#Creation of stop words that have no bearing
custom_stop <- tribble(
  ~word, ~lexicon,
  "ecq", "CUSTOM",
  "covid","CUSTOM",
  "quarantine","CUSTOM",
  "19","CUSTOM",
  "lang","CUSTOM",
  "ang","CUSTOM",
  "gcq","CUSTOM",
  "natin","CUSTOM"
  "na","CUSTOM",
  "hindi","CUSTOM")

#Tokenizing of articles
cnn_tokens <- cnn %>%
  unnest_tokens(word, cnn) %>%
  anti_join(stop_words) %>%
  anti_join(custom_stop) %>%
  count(word, sort=TRUE)
cnn_tokens

msn_tokens <- msn %>%
  unnest_tokens(word, msn) %>%
  anti_join(stop_words) %>%
  anti_join(custom_stop) %>%
  count(word, sort=TRUE)
msn_tokens

star_tokens <- star %>%
  unnest_tokens(word, star) %>%
  anti_join(stop_words) %>%
  anti_join(custom_stop) %>%
  count(word, sort=TRUE)
star_tokens
