# Setup

library(dplyr)
library(tidyr)
library(ggplot2)
library(tidytext)
library(wordcloud)
library(tm)

library(rstudioapi)

rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

lincoln_letter <- readLines('lincoln.txt')
lincoln_text <- Corpus(VectorSource(lincoln_letter))

# Clean text

lincoln_clean <- tm_map(lincoln_text, removePunctuation)
lincoln_clean <- tm_map(lincoln_clean, content_transformer(tolower))
lincoln_clean <- tm_map(lincoln_clean, removeNumbers)
lincoln_clean <- tm_map(lincoln_clean, stripWhitespace)
lincoln_clean <- tm_map(lincoln_clean, removeWords, stopwords('english'))

# Convert to Term Document Matrix

td_mat <- TermDocumentMatrix(lincoln_clean)
matrix <- as.matrix(td_mat)
sorted <- sort(rowSums(matrix), decreasing = T)
data_text <- data.frame(word = names(sorted), freq = sorted)

# Preview data

head(data_text, 30)

# Wordcloud with colours

set.seed(1234)
wordcloud(words = data_text$word, freq = data_text$freq, min.freq = 5,
          max.words = 100, random.order = F, rot.per = 0.35,
          colors = rainbow(30))

# Wordcloud with colours with lower max words and raise minimum frequency

wordcloud(words = data_text$word, freq = data_text$freq, min.freq = 15,
          max.words = 80, random.order = F, rot.per = 0.35,
          colors = rainbow(30))

## The Most Common Words In The Letter

# Wordcounts Plot
# ggplot2 bar plot (Top 25 words)

data_text[1:25, ] %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col(fill = "lightblue") +
  coord_flip() +
  labs(x = "Word \n", y = "\n Count ", title = "Word Counts (Top 25)") +
  geom_text(aes(label = freq), hjust = 1.2, colour = "black", fontface = "bold", size = 3.7) +
  theme(plot.title = element_text(hjust = 0.5, colour = "darkgreen", size = 15),
  axis.title.x = element_text(face = "bold", colour = "darkblue", size = 12),
  axis.title.y = element_text(face = "bold", colour = "darkblue", size = 12),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank())

# Sentiment analysis

lincoln_words <- data_frame(Text = lincoln_letter) %>%
  unnest_tokens(output = word, input = Text)

# Retrieve word counts as set up for sentiment lexicons

lincoln_wordcounts <- lincoln_words %>%
  anti_join(stop_words) %>%
  count(word, sort = T)

# Using nrc, bing and AFINN lexicons

word_labels_nrc <- c(`negative` = "Negative Words",
                     `positive` = "Positive Words")

# nrc lexicons
# get_sentiments('nrc')

lincoln_words_nrc <- lincoln_wordcounts %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  filter(sentiment %in% c("positive", "negative"))

head(lincoln_words_nrc, n = 30)

# sentiment plot with nrc lexicon (Word Count over 5)

lincoln_words_nrc %>%
  filter(n > 5) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = sentiment)) +
  geom_bar(stat = "identity", position = "identity") +
  geom_text(aes(label = n), colour = 'black', hjust = 1, fontface = 'bold', size = 3) +
  facet_wrap(~sentiment, nrow = 2, scales = 'free_y', labeller = as_labeller(word_labels_nrc)) +
  labs(x = '\n Word \n', y = '\n Word Count ', title = 'Negative & Positive Words in Mobey Dick') +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(face = 'bold', colour = 'darkblue', size = 12),
        axis.title.y = element_text(face = 'bold', colour = 'darkblue', size = 12),
        strip.background = element_rect(fill = 'lightblue'),
        strip.text.x = element_text(size = 10, face = 'bold')) +
  scale_fill_manual(values = c('#FF0000', '#01DF3A'), guide = F) +
  coord_flip()
