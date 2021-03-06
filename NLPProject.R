##installing packages dependencies and language model if not done yet
#install.packages("rvest")
#install.packages("dplyr")
#install.packages("utf8")
#install.packages("spacyr")
#spacy_install()
#spacy_download_langmodel('en') 
#install.packages("syuzhet")
#install.packages("ggplot2")
#install.packages("tidytext")
#install.packages("tidyverse")
#install.packages("textdata")
#install.packages("wordcloud")
#install.packages("RColorBrewer")

#loading libraries
library(rvest)
library(dplyr)
library(utf8)
library(spacyr)
library(syuzhet)
library(ggplot2)
library(tidytext)
library(tidyverse)
library(textdata)
library(wordcloud)
library(RColorBrewer)


##Webscraping the two relevant news articles
#create new variable for the link and get html document of this webpage
nypostlink = "https://nypost.com/2022/01/11/bidens-disgraceful-lies-on-filibuster-dems-power-grab-over-us-voting-laws/"
nypostpage = read_html(nypostlink)

#Scrape the article from the webpage using pipe operators with CSS tag from SelectorGadget
nypostarticle = nypostpage %>% html_nodes("p") %>% html_text()
nypostarticle

#repeat for the second article
thehilllink = "https://thehill.com/opinion/campaign/589286-bidens-georgia-speech-was-a-call-to-save-democracy-as-we-know-it?rl=1"
thehillpage = read_html(thehilllink)

thehillarticle = thehillpage %>% html_nodes("p") %>% html_text()
thehillarticle

#remove lines that are not part of the main article
nypostarticle = nypostarticle[2:13]
nypostarticle

thehillarticle = thehillarticle[2:16]
thehillarticle

##Some basic checks
#Check UTF-8 encoding: aiming for character(0)
nypostarticle[!utf8_valid(nypostarticle)] 
thehillarticle[!utf8_valid(thehillarticle)]

#Check character normalization. Specifically, the normalized composed form (NFC): 0 means all right. The text is in NFC.
nypostarticle_NFC <- utf8_normalize(nypostarticle)
sum(nypostarticle_NFC != nypostarticle)
thehillarticle_NFC <- utf8_normalize(thehillarticle)
sum(thehillarticle_NFC != thehillarticle)

#replace doubled or more spaces by unique spaces
nypostarticle <- gsub("[ ]{2,}", " ", nypostarticle)
thehillarticle <- gsub("[ ]{2,}", " ", thehillarticle)

#Gets single non-empty sentences/phrases from the extracted paragraphs of NYPost article
nypostarticlephrases <- spacy_tokenize(nypostarticle, what="sentence")
v_nypostarticlephrases <- unlist(nypostarticlephrases)
length(v_nypostarticlephrases) #21 sentences
sum(v_nypostarticlephrases=="") #1 sentence empty
v_nypostarticlephrases <- v_nypostarticlephrases[-which(v_nypostarticlephrases=="")] #20 sentences
length(v_nypostarticlephrases)

#repeat for TheHill article
thehillarticlephrases <- spacy_tokenize(thehillarticle, what ="sentence")
v_thehillarticlephrases <- unlist(thehillarticlephrases)
length(v_thehillarticlephrases) #47
sum(v_thehillarticlephrases=="") #16 sentences empty
v_thehillarticlephrases <- v_thehillarticlephrases[-which(v_thehillarticlephrases=="")] #31 sentences
length(v_thehillarticlephrases)

#obtaining sentiment scores for NYPost article and plotting them
sentimentscoresnypost <- get_nrc_sentiment(v_nypostarticlephrases)

cbind(v_nypostarticlephrases, sentimentscoresnypost)

barplot(sort(colSums(sentimentscoresnypost), decreasing = TRUE), las=2, col = rainbow(10), ylab = 'count', main ='Sentiment Scores for NYPost article')

#obtaining sentiment scores for TheHill article and plotting them
sentimentscoresthehill <- get_nrc_sentiment(v_thehillarticlephrases)

cbind(v_thehillarticlephrases, sentimentscoresthehill)

barplot(sort(colSums(sentimentscoresthehill), decreasing = TRUE), las=2, col = rainbow(10), ylab = 'count', main ='Sentiment Scores for TheHill article')


#combining both sentiment scores in one dataframe to plot a two-keyed barplot

sentimentdf <- data.frame(cbind(colSums(sentimentscoresnypost), colSums(sentimentscoresthehill))) 

sentimentdfnyp <- data.frame(
  "article" = "NYPost",
  "sentiments" = c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust", "negative", "positive"),
  "scores" = sentimentdf[ ,1])
sentimentdfthehill <- data.frame(
  "article" = "TheHill",
  "sentiments" = c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust", "negative", "positive"),
  "scores" = sentimentdf[ ,2])
sentimentdfboth <- rbind(sentimentdfnyp, sentimentdfthehill)

##plotting a two-key barplot to compare the articles absolutes scores
ggplot(sentimentdfboth, aes(x = sentiments, y = scores, fill = article)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title="Multiple Bar plots")

#modyfing the scores to relative scores for better comparability
sentimentdfnyp <- sentimentdfnyp %>% mutate_at(c("scores"), funs(relativescore = ./sum(scores)))
sentimentdfthehill <- sentimentdfthehill %>% mutate_at(c("scores"), funs(relativescore = ./sum(scores)))
sentimentdfboth <- rbind(sentimentdfnyp, sentimentdfthehill)

##plotting a two-key barplot to compare the articles relative scores
ggplot(sentimentdfboth, aes(x = sentiments, y = relativescore, fill = article)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title="Multiple Bar plots Relativescore")


## sentiment analysis with the tidytext package 
## getting some insights on top words for sentiments
#for NYPostarticle get word with count of sentiment from bing lexicon
nyposttext.df <- tibble(text = str_to_lower(v_nypostarticlephrases))
nypostbingwordcount <- nyposttext.df %>% unnest_tokens(output = word, input = text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) 

#WordCloud for the bing sentiments
par(mar=c(1,1,1,1))
wordcloud(words = filter(nypostbingwordcount, sentiment == "positive")$word, freq = nypostbingwordcount$n, min.freq = 1, max.words = 100, colors=brewer.pal(8, "Dark2"))
title(main= "NYPost article positive bing sentiment wordcloud", font.main = 1, cex.main = 1.5)
wordcloud(words = filter(nypostbingwordcount, sentiment == "negative")$word, freq = nypostbingwordcount$n, min.freq = 1, max.words = 100, colors=brewer.pal(8, "Dark2"))
title(main= "NYPost article negative bing sentiment wordcloud", font.main = 1, cex.main = 1.5)

# Top 10 words by sentiment for bing nypost
nyposttop10bingwords <- nypostbingwordcount %>% 
  group_by(sentiment) %>% 
  slice_max(order_by = n, n=10, with_ties = FALSE) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) 

# create a barplot showing the Top 10 words by sentiment for bing nypost
nyposttop10bingwords %>% 
  ggplot(aes(word, n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") + 
  labs(y = "Top 10 words by Sentiment for NYPost article with bing lexicon", x = NULL) + 
  coord_flip() 

#repeating for TheHill article
thehilltext.df <- tibble(text = str_to_lower(v_thehillarticlephrases))
thehillbingwordcount <- thehilltext.df %>% unnest_tokens(output = word, input = text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) 

wordcloud(words = filter(thehillbingwordcount, sentiment == "positive")$word, freq = thehillwordcount$n, min.freq = 1, max.words = 100, colors=brewer.pal(8, "Dark2"))
title(main= "TheHill article positive bing sentiment wordcloud", font.main = 1, cex.main = 1.5)
wordcloud(words = filter(thehillbingwordcount, sentiment == "negative")$word, freq = thehillwordcount$n, min.freq = 1, max.words = 100, colors=brewer.pal(8, "Dark2"))
title(main= "TheHill article negative bing sentiment wordcloud", font.main = 1, cex.main = 1.5)

thehilltop10bingwords <- thehillbingwordcount %>% 
  group_by(sentiment) %>% 
  slice_max(order_by = n, n = 10,with_ties = FALSE) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) 

thehilltop10bingwords %>% 
  ggplot(aes(word, n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") + 
  labs(y = "Top 10 words by Sentiment for TheHill article with bing lexicon", x = NULL) + 
  coord_flip() 

#for NYPostarticle get word with count of sentiment from loughran lexicon
nypostloughranwordcounts <- nyposttext.df %>% unnest_tokens(output = word, input = text) %>%
  inner_join(get_sentiments("loughran")) %>%
  count(word, sentiment, sort = TRUE) 

# Top 10 words by sentiment for laughran nypost
nyposttop10loughranwords <- nypostloughranwordcounts %>% 
  group_by(sentiment) %>% 
  slice_max(order_by = n, n = 10, with_ties = FALSE) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n))

# create a barplot showing the Top 10 words by sentiment for loughran nypost
nyposttop10loughranwords %>% 
  ggplot(aes(word, n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") + 
  labs(y = "Top 10 words by Sentiment for NYPost article with loughran lexicon", x = NULL) + 
  coord_flip() 

#repeating for theHill article
thehillloughranwordcounts <- thehilltext.df %>% unnest_tokens(output = word, input = text) %>%
  inner_join(get_sentiments("loughran")) %>%
  count(word, sentiment, sort = TRUE) 

thehilltop10loughranwords <- thehillloughranwordcounts %>% 
  group_by(sentiment) %>% 
  slice_max(order_by = n, n = 10, with_ties = FALSE) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n))

thehilltop10loughranwords %>% 
  ggplot(aes(word, n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") + 
  labs(y = "Top 10 words by Sentiment for TheHill article with loughran lexicon", x = NULL) + 
  coord_flip() 
