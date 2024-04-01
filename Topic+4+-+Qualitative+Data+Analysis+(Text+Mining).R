#-------------------------------------------------------------------------------
# Topic 4 - Qualitative Data Analysis (Text Mining)
#-------------------------------------------------------------------------------
# 1. Load Libraries

library(tidyverse)
library(tibble)
library(tidyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(lubridate)
library(tidytext)
library(wordcloud2)
library(stringr)
library(textdata)

#-------------------------------------------------------------------------------
#2. Import Text File
text <- readLines(file.choose())
text

#-------------------------------------------------------------------------------
#3. Tibble it (Convert to Dataframe)
# In order to turn it into a tidy text dataset, 
# we first need to put it into a data frame.

length = length(text)
text_df <- tibble(line = 1:length, text = text)
text_df

#-------------------------------------------------------------------------------
#5. Tokenize
tidy_df <- text_df %>%
  unnest_tokens(word, text)
tidy_df

#-------------------------------------------------------------------------------
#6.  Remove the stop words
tidy_df_rm <- tidy_df %>%
  anti_join(stop_words)
tidy_df_rm

#-------------------------------------------------------------------------------
#7. Count the most frequent words
tidy_df_rm %>%
  count(word, sort = TRUE) 

#-------------------------------------------------------------------------------
#8. Activity: Import + Tibble + Stop Words + Count

#https://www.congress.gov/congressional-record/2009/09/22/senate-section/article/s9648-2/
  
#8a. Import
text <- readLines(file.choose())
text

#8b. Tibble
length = length(text)
text_df <- tibble(line = 1:length, text = text)
text_df

#8c. Tokenize
tidy_df <- text_df %>%
  unnest_tokens(word, text)
tidy_df

#8d. Remove Stop Words
tidy_df_rm <- tidy_df %>%
  anti_join(stop_words)
tidy_df

#8e. Count Most Frequent Words
tidy_df_rm %>%
  count(word, sort = TRUE) 

#-------------------------------------------------------------------------------
#9. Word Cloud

tidy_df_rm %>%
  count(word, sort=TRUE) %>%
  wordcloud2(word,size=2)

#-------------------------------------------------------------------------------
#10. Activity: Word Cloud

#10a. Import 
text <- readLines(file.choose())
text

#10b. Tibble
length = length(text)
text_df <- tibble(line = 1:length, text = text)
text_df

#10c. Tokenize
tidy_df <- text_df %>%
  unnest_tokens(word, text)
tidy_df

#10d. Remove Stop Words
tidy_df_rm <- tidy_df %>%
  anti_join(stop_words)
tidy_df

#10e. Word Cloud
tidy_df_rm %>%
  count(word, sort=TRUE) %>%
  wordcloud2(word,size=2)

#-------------------------------------------------------------------------------
#11. NRC Sentiment Analysis
nrc_sentiment <- get_sentiments("nrc") %>% 
  filter(sentiment == "fear")

#inner join nrc_sentiment
tidy_df_rm %>%
  inner_join(nrc_sentiment) %>%
  count(word, sort = TRUE)

#-------------------------------------------------------------------------------
#12. Activity: NRC Sentiment Analysis

#12a. Import
text <- readLines(file.choose())
text

#12b. Tibble
length = length(text)
text_df <- tibble(line = 1:length, text = text)
text_df

#12c. Tokenize
tidy_df <- text_df %>%
  unnest_tokens(word, text)
tidy_df

#12d. Remove Stop Words 
tidy_df_rm <- tidy_df %>%
  anti_join(stop_words)
tidy_df_rm

#12e. Inner Join nrc_sentiment
tidy_df_rm %>%
  inner_join(nrc_sentiment) %>%
  count(word, sort = TRUE)

#-------------------------------------------------------------------------------
#13. Visualize NRC Sentiment
tidy_df_rm %>%
  filter(n > 0) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill=n)) + theme_fivethirtyeight() + geom_col() +
  xlab(NULL) + coord_flip() + ylab("Word Count") + 
  ggtitle("Fear Words Usage in Eisenhower",
          subtitle = "Sentiment Analysis Using NRC")

#-------------------------------------------------------------------------------
#14. BING Sentiment Analysis 
#Bing categorizes words as Positive or Negative

#14a. Get Sentiments
bing = get_sentiments('bing')

#14b. Inner Join Sentiments to Text File
tb_un_rm_bing = tb_un_rm %>%
  inner_join(get_sentiments('bing'))

#14c. Do Word Count + Inner Join Sentiments to Text File
tb_un_rm_bing_1 = tb_un_rm_bing %>%
  count(word, sort = TRUE) %>%
  inner_join(tb_un_rm_bing)

#14d. Visualize
tb_un_rm_bing_1 %>%
  filter(n > 0) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) + theme_fivethirtyeight() + geom_col() +
  xlab(NULL) + coord_flip() + ylab("Word Count") + 
  ggtitle("Positive / Negative Words Usage in Eisenhower",
          subtitle = "Sentiment Analysis Using Bing et al.")

#-------------------------------------------------------------------------------
#15. Activity: BING Sentiment Analysis

#-------------------------------------------------------------------------------
#16. Activity: AFINN sentiment Analysis
# AFINN give word scores
# -5 is Negative while 5 is Positive

#-------------------------------------------------------------------------------
#THE END
#-------------------------------------------------------------------------------