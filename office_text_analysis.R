library(tidyverse)
library(read_excel)
library(tidytext)
library(textdata)

the_office_lines <- read_excel("the-office-lines.xlsx")
initial_data <- the_office_lines

mod_data <- initial_data %>% 
  filter(deleted == "FALSE") %>% 
  mutate(actions = str_extract_all(line_text, "\\[.*?\\]"),
         line_text_mod = str_trim(str_replace_all(line_text, "\\[.*?\\]", ""))) %>% 
  mutate_at(vars(line_text_mod), funs(str_replace_all(., "���","'"))) %>% 
  mutate_at(vars(speaker), funs(tolower)) %>% 
  mutate_at(vars(speaker), funs(str_trim(str_replace_all(., "\\[.*?\\]", "")))) %>% 
  mutate_at(vars(speaker), funs(str_replace_all(., "micheal|michel|michae$", "michael")))


## Need to render each of the words into its own line
text_df_original <- mod_data %>% unnest_tokens(word, line_text_mod)

## Words which are used most often during the show, but first get rid of common words. Conveniently, these are stored in the data(stop_words) and can be removed using anti-join

data(stop_words)

text_df <- text_df_original %>% anti_join(stop_words)

array_of_words <- text_df %>% count(word, sort=TRUE)


## How many times is michael's name used after season 7? 13
text_df %>% subset(text_df$season > 7) %>% count(word=="michael", sort=TRUE)

## gutenbergr is a nice package for looking at data from a bunch of open source books

frequency <- text_df %>% group_by(season)

nrc_joy <- get_sentiments("nrc") %>% filter(sentiment == "joy")

## What are the words typically associated with joy which michael most often uses? Recall that analyses like this don't take into account sarcasm because it considers the individual words as unigrams
text_df %>%
  filter(speaker == "michael") %>%
  inner_join(nrc_joy) %>%
  count(word, sort=TRUE)

































