#installing and loading the mongolite library to download the Airbnb data
#install.packages("mongolite") #need to run this line of code only once and then you can comment out
library(mongolite)
# This is the connection_string. You can get the exact url from your MongoDB cluster screen
#replace the <<user>> with your Mongo user name and <<password>> with the mongo password
#lastly, replace the <<server_name>> with your MongoDB server name
connection_string <- 'mongodb+srv://*:*@cluster0.bhxad.mongodb.net/Cluster0?retryWrites=true&w=majority'
airbnb_collection <- mongo(collection="listingsAndReviews", db="sample_airbnb", url=connection_string)

#Here's how you can download all the Airbnb data from Mongo
## keep in mind that this is huge and you need a ton of RAM memory

airbnb_all <- airbnb_collection$find()

#Adding the country column
country <- airbnb_all$address[,5]
airbnb_all$country <- country

#Creating a new variable for the price without the currency
airbnb_spain <- airbnb_all %>% 
  filter(country == "Spain") %>% 
  mutate(priceUSD = price * 1.13, country = "Spain")
  
airbnb_brazil <- airbnb_all %>% 
  filter(country == "Brazil") %>% 
  mutate(priceUSD = price * 0.18, country = "Brazil")
  
airbnb_portugal <- airbnb_all %>% 
  filter(country == "Portugal") %>% 
  mutate(priceUSD = price * 1.13, country = "Portugal") 

airbnb_canada <- airbnb_all %>% 
  filter(country == "Canada") %>% 
  mutate(priceUSD = price * 0.79, country = "Canada") 

airbnb_turkey <- airbnb_all %>% 
  filter(country == "Turkey") %>% 
  mutate(priceUSD = price * 0.072, country = "Turkey") 

airbnb_hk <- airbnb_all %>% 
  filter(country == "Hong Kong") %>% 
  mutate(priceUSD = price * 0.13, country = "Hong Kong")

airbnb_australia <- airbnb_all %>% 
  filter(country == "Australia") %>% 
  mutate(priceUSD = price * 0.72, country = "Australia")

airbnb_china <- airbnb_all %>% 
  filter(country == "China") %>% 
  mutate(priceUSD = price * 0.16, country = "China")

airbnb_usa <- airbnb_all %>% 
  filter(country == "United States") %>% 
  mutate(priceUSD = price,country = "United States")
 
airbnb_all <- bind_rows(airbnb_portugal, airbnb_brazil, airbnb_usa, airbnb_turkey, airbnb_china, airbnb_hk, airbnb_spain, airbnb_australia, airbnb_canada)

#We want to add the language column to the data frame
library(textcat)

airbnb_all[airbnb_all$summary == " "] <- "NA"
na.omit(airbnb_all$summary)

language <- c()
language <- textcat(airbnb_all$summary)
airbnb_all$language <- language

#We create new data frames for french, spanish, english to analyze the data frame
library(dplyr)


english_df <- airbnb_all %>% 
  filter(language == "english")


spanish_df <- airbnb_all %>% 
  filter(language == "spanish")

french_df <- airbnb_all %>% 
  filter(language == "french")

#We want to understand more about the summaries and description made in french
#This part is focus only in french summaries and descriptions
french_summary<- french_df$summary
french_summary <- data.frame(line=1:221, text=french_summary)
french_summary <- cbind(french_summary, french_df$country)
col <- c("line", "text", "country")
colnames(french_summary) <- col

french_description <- french_df$description
french_description <- data.frame(line=1:221, text=french_description)
french_description <- cbind(french_description, french_df$country)
colnames(french_description) <- col

#We want to have the french stop_words
library(tidystopwords)
library(tidytext)

my_french_stopswords <- generate_stoplist(language = "French")
my_french_stopswords <- data.frame(word = my_french_stopswords, stringsAsFactors = FALSE)

#We are going to tokenize
token_summary <- french_summary %>% 
  unnest_tokens(word, text) %>% 
  anti_join(my_french_stopswords) %>% 
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

token_description <- french_description %>% 
  unnest_tokens(word, text) %>% 
  anti_join(my_french_stopswords) %>%
  anti_join(stop_words) %>% 
  count(word, sort = TRUE)

#from this tokenization we create a wordcloud
#Doing a wordcloud to see what is associated with sustainable fashion
#install.packages("wordcloud2")
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)

token_summary_100 <- token_summary %>%
  top_n(100)

summary_wordcloud <-wordcloud2(data = token_summary_100, 
           size = 0.6, color = "random-dark")

token_description_100 <- token_description %>%
  top_n(100)

description_wordcloud <-wordcloud2(data = token_description_100, 
                               size = 0.6, color = "random-dark")

#Let's work on the N Grams for each of one of them
#Let's start with summary
summary_trigrams <- french_summary %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3)

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
summary_trigrams_separated <- summary_trigrams %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")

summary_trigrams_filtered <- summary_trigrams_separated %>%
  filter(!word1 %in% my_french_stopswords$word) %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% my_french_stopswords$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% my_french_stopswords$word) %>% 
  filter(!word3 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
summary_counts <- summary_trigrams_filtered %>%
  count(word1, word2, word3, sort = TRUE)
######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################
#install.packages("igraph")
library(igraph)
summary_trigram_graph <- summary_counts %>%
  filter(n>1) %>%
  graph_from_data_frame()

summary_trigram_graph

#install.packages("ggraph")
library(ggraph)
ggraph(summary_trigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

#let's continue with the description
#Let's work on the N Grams for each of one of them
description_quadrograms <- french_description %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4)

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
description_quadrograms_separated <- description_quadrograms %>%
  separate(quadrogram, c("word1", "word2","word3", "word4"), sep = " ")

description_quadrograms_filtered <- description_quadrograms_separated %>%
  filter(!word1 %in% my_french_stopswords$word) %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% my_french_stopswords$word) %>% 
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% my_french_stopswords$word) %>% 
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% my_french_stopswords$word) %>%
  filter(!word4 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
description_counts <- description_quadrograms_filtered %>%
  count(word1, word2, word3, word4, sort = TRUE)
######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################
#install.packages("igraph")
library(igraph)
description_quadrogram_graph <- description_counts %>%
  filter(n>1) %>%
  graph_from_data_frame()

description_quadrogram_graph

#install.packages("ggraph")
library(ggraph)
library(dplyr)

ggraph(description_quadrogram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

#TF_IDF
#we're grouping by the country this time
summary_token <- french_summary %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% my_french_stopswords$word) %>%
  filter(!word %in% stop_words$word) %>%
  count(country, word, sort=TRUE) %>%
  ungroup()

total_words <- summary_token %>%
  group_by(country) %>%
  summarise(total=sum(n))

summary_words <- left_join(summary_token, total_words)
  

print(summary_words)

library(ggplot2)
ggplot(summary_words, aes(n/total, fill = country))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.001) +
  facet_wrap(~country, ncol=2, scales="free_y")

country_words_summary <- summary_words %>%
  bind_tf_idf(word, country, n)

country_words_summary # we get all the zeors because we are looking at stop words ... too common

country_words_summary %>%
  arrange(desc(tf_idf))

#############
# looking at the graphical approach:
country_words_summary %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(country) %>%
  top_n(5) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=country))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~country, ncol=2, scales="free")+
  coord_flip()

#same for description
description_token <- french_description %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% my_french_stopswords$word) %>%
  filter(!word %in% stop_words$word) %>%
  count(country, word, sort=TRUE) %>%
  ungroup()

total_words2 <- description_token %>%
  group_by(country) %>%
  summarise(total=sum(n))

description_words <- left_join(description_token, total_words2)


print(description_words)

library(ggplot2)
ggplot(description_words, aes(n/total, fill = country))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.001) +
  facet_wrap(~country, ncol=2, scales="free_y")

country_words_description <- description_words %>%
  bind_tf_idf(word, country, n)

country_words_description # we get all the zeors because we are looking at stop words ... too common

country_words_description %>%
  arrange(desc(tf_idf))

#############
# looking at the graphical approach:
country_words_description %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(country) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=country))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~country, ncol=2, scales="free")+
  coord_flip()

#Focusing on the notes and rules in French
french_notes<- french_df$notes
french_notes <- data.frame(line=1:221, text=french_notes)
french_notes <- cbind(french_notes, french_df$country)
col <- c("line", "text", "country")
colnames(french_notes) <- col

fdflanguagerules <- textcat(french_df$house_rules)
french_df$languagerules <- fdflanguagerules

french_houserules <- french_df %>% 
  filter(languagerules == "english"  | languagerules == "french" ) %>% 
  select(house_rules, country)
french_houserules <- data.frame(line=1:87, text=french_houserules)

colnames(french_houserules) <- col


#We are going to tokenize
token_notes <- french_notes %>% 
  unnest_tokens(word, text) %>% 
  anti_join(my_french_stopswords) %>% 
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

token_houserules <- french_houserules %>% 
  unnest_tokens(word, text) %>% 
  anti_join(my_french_stopswords) %>%
  anti_join(stop_words) %>% 
  count(word, sort = TRUE)

#from this tokenization we create a wordcloud
#Doing a wordcloud to see what is associated with sustainable fashion
#install.packages("wordcloud2")
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)

token_notes_100 <- token_notes %>%
  top_n(100)

notes_wordcloud <-wordcloud2(data = token_notes_100, 
                               size = 0.6, color = "random-dark")

token_houserules_100 <- token_houserules %>%
  top_n(100)

houserules_wordcloud <-wordcloud2(data = token_houserules_100, 
                                   size = 0.6, color = "random-dark")

#Let's work on the N Grams for each of one of them
#Let's start with summary
notes_bigrams <- french_notes %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
notes_bigrams_separated <- notes_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

notes_bigrams_filtered <- notes_bigrams_separated %>%
  filter(!word1 %in% my_french_stopswords$word) %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% my_french_stopswords$word) %>%
  filter(!word2 %in% stop_words$word) 

#creating the new bigram, "no-stop-words":
notes_counts <- notes_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################
#install.packages("igraph")
library(igraph)
notes_bigram_graph <- notes_counts %>%
  filter(n>1) %>%
  graph_from_data_frame()

notes_bigram_graph

#install.packages("ggraph")
library(ggraph)
ggraph(notes_bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

#let's continue with the description
#Let's work on the N Grams for each of one of them
houserules_bigrams <- french_houserules %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
houserules_bigrams_separated <- houserules_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

houserules_bigrams_filtered <- houserules_bigrams_separated %>%
  filter(!word1 %in% my_french_stopswords$word) %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% my_french_stopswords$word) %>% 
  filter(!word2 %in% stop_words$word) 

#creating the new bigram, "no-stop-words":
houserules_counts <- houserules_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################
#install.packages("igraph")
library(igraph)
houserules_bigram_graph <- houserules_counts %>%
  filter(n>1) %>%
  graph_from_data_frame()

houserules_bigram_graph

#install.packages("ggraph")
library(ggraph)
library(dplyr)

ggraph(houserules_bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

#Correlelograms
library(tidyr)
library(tidytext)
library(stringr)
frequency1 <- bind_rows(mutate(token_notes, author ="Notes"),
                       mutate(token_houserules, author = "House Rules")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `House Rules`)

#let's plot the correlograms:
library(scales)
library(plotly)
plotlycor2 <-ggplot(frequency1, aes(x=proportion, y=`Notes`, 
                                  color = abs(`Notes`- proportion), key=word))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Notes", x=NULL)

plotlycor2 <- ggplotly(plotlycor2, tooltip = c("key", "proportion"))

#Review analysis 
#install.packages("splitstackshape")
library(splitstackshape)

#creating a data frame of reviews
reviews <- airbnb_all$reviews
reviews <- Filter(function(x) dim(x) [1] > 0, reviews)

reviews <- bind_rows(reviews)

#Splitting the first column to be able to link it
airbnb_all <- cSplit(airbnb_all, "listing_url", "/")

#convert each
reviews$listing_id <- as.numeric(reviews$listing_id)
airbnb_all$listing_url_5 <- as.numeric(airbnb_all$listing_url_5)

#create an inner join to create a new data frame
airbnbwithreviews <- airbnb_all%>% 
  inner_join(reviews, by = c("listing_url_5" = "listing_id"))

#Checking the languages of the reviews
languagereviews <- c()
languagereviews <- textcat(airbnbwithreviews$comments)

airbnbwithreviews$languagereviews <- languagereviews

englishreviews <- airbnbwithreviews %>% 
  filter(languagereviews == "english")
  
frenchreviews <- airbnbwithreviews %>% 
  filter(languagereviews == "french")  

spanishreviews <- airbnbwithreviews %>% 
  filter(languagereviews == "spanish")

#Analysisi of the superhosts comments in english
commentsuperhosten <- englishreviews %>% 
  group_by(host.host_is_superhost) %>% 
  select(comments)

colnames(commentsuperhosten)[2] <- "text"

reviewsen_token <- commentsuperhosten %>%
  filter(host.host_is_superhost == TRUE) %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort=T)

########################################################
##### NRC reviews####
########################################################
nrc <- reviewsen_token%>%
    inner_join(get_sentiments("nrc") %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment)

#english reviews n grams
englishreviews_trigrams <- commentsuperhosten %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3)

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
englishreview_trigrams_separated <- englishreviews_trigrams %>%
  separate(trigram, c("word1", "word2","word3"), sep = " ")

englishreview_trigrams_filtered <- englishreview_trigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
englishreview_counts <- englishreview_trigrams_filtered %>%
  count(word1, word2, word3, sort = TRUE)
######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################
#install.packages("igraph")
library(igraph)
englishreview_trigram_graph <- englishreview_counts %>%
  filter(n>25) %>%
  graph_from_data_frame()

englishreview_trigram_graph

#install.packages("ggraph")
library(ggraph)
library(dplyr)

ggraph(englishreview_trigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

#####################
#Analyzing the reviews in french with sentimental analysis 
library(tidyverse)
library(tidytext)
library(tm)

#Loading the lexicon
FEEL_lex <- read_csv2("http://advanse.lirmm.fr/FEEL.csv")

#Filtering for the super hosts
commentsuperhostfr <- frenchreviews %>% 
  filter(host.host_is_superhost == TRUE) %>% 
  select(comments)

opinions1 <- commentsuperhostfr$comments

#Cleaning the data
test1 <- stripWhitespace(opinions1)
test_df <- data_frame(text = test1) %>% unnest_tokens(word, text)
test_clean_df <- test_df %>% 
  filter(!word %in% stopwords('french')) %>% 
  filter(!word %in% stop_words$word)

#Comparing with lexicon to get positive, negative and other emotions in text
test_feel_df <- inner_join(test_clean_df, FEEL_lex, by = 'word')
sum(test_feel_df['polarity']=='positive')
sum(test_feel_df['polarity']=='negative')
sum(test_feel_df$joy)

sentiments_scorecard <- data.frame(
  Total_Positive = sum(test_feel_df$polarity == 'positive'),
  Total_Negative = sum(test_feel_df$polarity == 'negative'),
  
  Total_Joy_Words = sum(test_feel_df$joy),
  Total_Fear_Words = sum(test_feel_df$fear),
  Total_Sadness_Words = sum(test_feel_df$sadness),
  Total_Anger_Words = sum(test_feel_df$anger),
  Total_Surprise_Words = sum(test_feel_df$surprise),
  Total_Disgust_Words = sum(test_feel_df$disgust))

sentiments_scorecard <- sentiments_scorecard %>%
  mutate(Polarity = Total_Positive-Total_Negative)

sentiments_scorecard

############
#Analyzing the sentiment of the reviews in  Spanish
library(tidyverse)
library(tidytext)
library(tm)
library(readxl)
library(dplyr)

#Loading the spanish lexicon
spanish_sentiments <- read_excel("Desktop/spanish sentiments.xlsx")
View(spanish_sentiments)

#filtering for the superhosts
commentsuperhostsp <- spanishreviews %>% 
  filter(host.host_is_superhost == TRUE) %>% 
  select(comments)

opinions2 <- commentsuperhostsp$comments

test2 <- stripWhitespace(opinions2)
test_df2 <- data_frame(text = test2) %>% unnest_tokens(word, text)
test_clean_df2 <- test_df2 %>% 
  filter(!word %in% stopwords('spanish')) %>% 
  filter(!word %in% stop_words$word)
#Comparing with lexicon to get positive, negative and other emotions in text
test_feel_df2 <- inner_join(test_clean_df2, spanish_sentiments, by = 'word')

sentiments_scorecard2 <- data.frame(
  Total_Joy_Words = sum(test_feel_df2['sentiment']=='joy'),
  Total_Fear_Words = sum(test_feel_df2['sentiment']=='fear'),
  Total_Sadness_Words = sum(test_feel_df2['sentiment']=='sadness'),
  Total_Anger_Words = sum(test_feel_df2['sentiment']=='anger'),
  Total_Surprise_Words = sum(test_feel_df2['sentiment']=='surprise'),
  Total_Disgust_Words = sum(test_feel_df2['sentiment']=='disgust'))

sentiments_scorecard2

############################# Spanish 
#We want to understand more about the summaries and description made in spanish
#This part is focus only in spanish summaries and descriptions
spanish_summary<- spanish_df$summary
spanish_summary <- data.frame(line=1:144, text=spanish_summary)
spanish_summary <- cbind(spanish_summary, spanish_df$country)
col <- c("line", "text", "country")
colnames(spanish_summary) <- col

spanish_description <- spanish_df$description
spanish_description <- data.frame(line=1:144, text=spanish_description)
spanish_description <- cbind(spanish_description, spanish_df$country)
colnames(spanish_description) <- col

#We want to have the spanish stop_words
library(tidystopwords)
library(tidytext)

my_spanish_stopswords <- generate_stoplist(language = "Spanish")
my_spanish_stopswords <- data.frame(word = my_spanish_stopswords, stringsAsFactors = FALSE)

#We are going to tokenize
token_summary_sp <- spanish_summary %>% 
  unnest_tokens(word, text) %>% 
  anti_join(my_spanish_stopswords) %>% 
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

token_description_sp <- spanish_description %>% 
  unnest_tokens(word, text) %>% 
  anti_join(my_spanish_stopswords) %>%
  anti_join(stop_words) %>% 
  count(word, sort = TRUE)

#from this tokenization we create a wordcloud
#Doing a wordcloud to see what is associated with sustainable fashion
#install.packages("wordcloud2")
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)

token_summary_100_sp <- token_summary_sp %>%
  top_n(100)

summary_wordcloud_sp <-wordcloud2(data = token_summary_100_sp, 
                                  size = 0.6, color = "random-dark")

token_description_100_sp <- token_description_sp %>%
  top_n(100)

description_wordcloud_sp <-wordcloud2(data = token_description_100_sp, 
                                      size = 0.6, color = "random-dark")

#Let's work on the N Grams for each of one of them
#Let's start with summary
summary_trigrams_sp <- spanish_summary %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3)

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
summary_trigrams_separated_sp <- summary_trigrams_sp %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")

summary_trigrams_filtered_sp <- summary_trigrams_separated_sp %>%
  filter(!word1 %in% my_spanish_stopswords$word) %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% my_spanish_stopswords$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% my_spanish_stopswords$word) %>% 
  filter(!word3 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
summary_counts_sp <- summary_trigrams_filtered_sp %>%
  count(word1, word2, word3, sort = TRUE)
######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################
#install.packages("igraph")
library(igraph)
summary_trigram_graph_sp <- summary_counts_sp %>%
  filter(n>1) %>%
  graph_from_data_frame()

summary_trigram_graph

#install.packages("ggraph")
library(ggraph)
ggraph(summary_trigram_graph_sp, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

# Description Spanish -----------------------------------------------------
#let's continue with the description
#Let's work on the N Grams for each of one of them
description_quadrograms_sp <- spanish_description %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4)
#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
description_quadrograms_separated_sp <- description_quadrograms_sp %>%
  separate(quadrogram, c("word1", "word2","word3", "word4"), sep = " ")

description_quadrograms_filtered_sp <- description_quadrograms_separated_sp %>%
  filter(!word1 %in% my_spanish_stopswords$word) %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% my_spanish_stopswords$word) %>% 
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% my_spanish_stopswords$word) %>% 
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% my_spanish_stopswords$word) %>%
  filter(!word4 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
description_counts_sp <- description_quadrograms_filtered_sp %>%
  count(word1, word2, word3, word4, sort = TRUE)
#Visual N.gram network
#install.packages("igraph")
library(igraph)
description_quadrogram_graph_sp <- description_counts_sp %>%
  filter(n>1) %>%
  graph_from_data_frame()

description_quadrogram_graph

#install.packages("ggraph")
library(ggraph)
library(dplyr)

ggraph(description_quadrogram_graph_sp, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

# Correlograms ------------------------------------------------------------
#Correlelograms
library(tidyr)
library(tidytext)
library(stringr)
frequency_sp <- bind_rows(mutate(token_summary_sp, author ="Summary"),
                          mutate(token_description_sp, author = "Description")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `Description`)

#let's plot the correlograms:
library(scales)
library(plotly)
plotlycor_sp <-ggplot(frequency_sp, aes(x=proportion, y=`Summary`, 
                                        color = abs(`Summary`- proportion), key=word))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Summary", x=NULL)

plotlycor_sp <- ggplotly(plotlycor_sp, tooltip = c("key", "proportion"))



# TF -IDF SPANISH ---------------------------------------------------------
#TF_IDF
#we're grouping by the country this time
summary_token_sp <- spanish_summary %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% my_spanish_stopswords$word) %>%
  filter(!word %in% stop_words$word) %>%
  count(country, word, sort=TRUE) %>%
  ungroup()

total_words_sp <- summary_token_sp %>%
  group_by(country) %>%
  summarise(total=sum(n))

summary_words_sp <- left_join(summary_token_sp, total_words_sp)


print(summary_words_sp)

library(ggplot2)
ggplot(summary_words_sp, aes(n/total, fill = country))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.001) +
  facet_wrap(~country, ncol=2, scales="free_y")

country_words_summary_sp <- summary_words_sp %>%
  bind_tf_idf(word, country, n)

country_words_summary_sp # we get all the zeors because we are looking at stop words ... too common

country_words_summary_sp %>%
  arrange(desc(tf_idf))

#############
# looking at the graphical approach:
country_words_summary_sp %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(country) %>%
  top_n(5) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=country))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~country, ncol=2, scales="free")+
  coord_flip()

#same for description
description_token_sp <- spanish_description %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% my_spanish_stopswords$word) %>%
  filter(!word %in% stop_words$word) %>%
  count(country, word, sort=TRUE) %>%
  ungroup()

total_words_desc_sp <- description_token_sp %>%
  group_by(country) %>%
  summarise(total=sum(n))

description_words_sp <- left_join(description_token_sp, total_words_desc_sp)


print(description_words)

library(ggplot2)
ggplot(description_words_sp, aes(n/total, fill = country))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.001) +
  facet_wrap(~country, ncol=2, scales="free_y")

country_words_description_sp <- description_words_sp %>%
  bind_tf_idf(word, country, n)

country_words_description # we get all the zeors because we are looking at stop words ... too common

country_words_description_sp %>%
  arrange(desc(tf_idf))

#############
# looking at the graphical approach:
country_words_description_sp %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(country) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=country))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~country, ncol=2, scales="free")+
  coord_flip()


# Spanish Notes -----------------------------------------------------------
#Question6
spanish_notes<- spanish_df$notes
spanish_notes <- data.frame(line=1:144, text=spanish_notes)
spanish_notes <- cbind(spanish_notes, spanish_df$country)
col <- c("line", "text", "country")
colnames(spanish_notes) <- col

fdflanguagerules <- textcat(spanish_df$house_rules)
spanish_df$languagerules <- fdflanguagerules

spanish_houserules <- spanish_df %>% 
  filter(languagerules == "english"  | languagerules == "spanish" ) %>% 
  select(house_rules, country)
spanish_houserules <- data.frame(line=1:74, text=spanish_houserules)

colnames(spanish_houserules) <- col


#We are going to tokenize
token_notes_sp <- spanish_notes %>% 
  unnest_tokens(word, text) %>% 
  anti_join(my_spanish_stopswords) %>% 
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

token_houserules_sp <- spanish_houserules %>% 
  unnest_tokens(word, text) %>% 
  anti_join(my_spanish_stopswords) %>%
  anti_join(stop_words) %>% 
  count(word, sort = TRUE)

#from this tokenization we create a wordcloud
#Doing a wordcloud to see what is associated with sustainable fashion
#install.packages("wordcloud2")
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)

token_notes_100_sp <- token_notes_sp %>%
  top_n(100)

notes_wordcloud_sp <-wordcloud2(data = token_notes_100_sp, 
                                size = 0.6, color = "random-dark")

token_houserules_100_sp <- token_houserules_sp %>%
  top_n(100)

houserules_wordcloud_sp <-wordcloud2(data = token_houserules_100_sp, 
                                     size = 0.6, color = "random-dark")

#Let's work on the N Grams for each of one of them
#Let's start with summary
notes_bigrams_sp <- spanish_notes %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
notes_bigrams_separated_sp <- notes_bigrams_sp %>%
  separate(bigram, c("word1", "word2"), sep = " ")

notes_bigrams_filtered_sp <- notes_bigrams_separated_sp %>%
  filter(!word1 %in% my_spanish_stopswords$word) %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% my_spanish_stopswords$word) %>%
  filter(!word2 %in% stop_words$word) 

#creating the new bigram, "no-stop-words":
notes_counts_sp <- notes_bigrams_filtered_sp %>%
  count(word1, word2, sort = TRUE)
######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################
#install.packages("igraph")
library(igraph)
notes_bigram_graph_sp <- notes_counts_sp %>%
  filter(n>1) %>%
  graph_from_data_frame()

notes_bigram_graph

#install.packages("ggraph")
library(ggraph)
ggraph(notes_bigram_graph_sp, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

#let's continue with the description
#Let's work on the N Grams for each of one of them
houserules_bigrams_sp <- spanish_houserules %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
houserules_bigrams_separated_sp <- houserules_bigrams_sp %>%
  separate(bigram, c("word1", "word2"), sep = " ")

houserules_bigrams_filtered_sp <- houserules_bigrams_separated_sp %>%
  filter(!word1 %in% my_spanish_stopswords$word) %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% my_spanish_stopswords$word) %>% 
  filter(!word2 %in% stop_words$word) 

#creating the new bigram, "no-stop-words":
houserules_counts_sp <- houserules_bigrams_filtered_sp %>%
  count(word1, word2, sort = TRUE)
######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################
#install.packages("igraph")
library(igraph)
houserules_bigram_graph_sp <- houserules_counts_sp %>%
  filter(n>1) %>%
  graph_from_data_frame()

houserules_bigram_graph

#install.packages("ggraph")
library(ggraph)
library(dplyr)

ggraph(houserules_bigram_graph_sp, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

#Correlelograms
library(tidyr)
library(tidytext)
library(stringr)
frequency_notes_hr_sp <- bind_rows(mutate(token_notes_sp, author ="Notes"),
                                   mutate(token_houserules_sp, author = "House Rules")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `House Rules`)

#let's plot the correlograms:
library(scales)
library(plotly)
plotlycor_sp <-ggplot(frequency_notes_hr_sp, aes(x=proportion, y=`Notes`, 
                                                 color = abs(`Notes`- proportion), key=word))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Notes", x=NULL)

plotlycor2 <- ggplotly(plotlycor_sp, tooltip = c("key", "proportion"))


# TF_IDF ------------------------------------------------------------------
#TF_IDF
#we're grouping by the country this time
notes_token_sp <- spanish_notes %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% my_spanish_stopswords$word) %>%
  filter(!word %in% stop_words$word) %>%
  count(country, word, sort=TRUE) %>%
  ungroup()

total_words_notes_sp <- notes_token_sp %>%
  group_by(country) %>%
  summarise(total=sum(n))

notes_words_sp <- left_join(notes_token_sp, total_words_notes_sp)


print(notes_words_sp)

library(ggplot2)
ggplot(notes_words_sp, aes(n/total, fill = country))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.001) +
  facet_wrap(~country, ncol=2, scales="free_y")

country_words_notes_sp <- notes_words_sp %>%
  bind_tf_idf(word, country, n)

country_words_notes_sp # we get all the zeors because we are looking at stop words ... too common

country_words_notes_sp %>%
  arrange(desc(tf_idf))


# looking at the graphical apprach:
country_words_notes_sp %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(country) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=country))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~country, ncol=2, scales="free")+
  coord_flip()




#################### English
#We want to understand more about the summaries and description made in english
#This part is focus only in english summaries and descriptions
#Question 5
english_summary<- english_df$summary
english_summary <- data.frame(line=1:3959, text=english_summary)
english_summary <- cbind(english_summary, english_df$country)
col <- c("line", "text", "country")
colnames(english_summary) <- col

english_description <- english_df$description
english_description <- data.frame(line=1:3959, text=english_description)
english_description <- cbind(english_description, english_df$country)
colnames(english_description) <- col

#We are going to tokenize
token_summary_eng <- english_summary %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

token_description_eng <- english_description %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE)

#from this tokenization we create a wordcloud
#Doing a wordcloud to see what is associated with sustainable fashion
#install.packages("wordcloud2")
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)

token_summary_100_eng <- token_summary_eng %>%
  top_n(100)

summary_wordcloud_eng <-wordcloud2(data = token_summary_100_eng, 
                                   size = 0.6, color = "random-dark")

token_description_100_eng <- token_description_eng %>%
  top_n(100)

description_wordcloud_eng <-wordcloud2(data = token_description_100_eng, 
                                       size = 0.6, color = "random-dark")

#Let's work on the N Grams for each of one of them
#Let's start with summary
summary_trigrams_eng <- english_summary %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3)

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
summary_trigrams_separated_eng <- summary_trigrams_eng %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")

summary_trigrams_filtered_eng <- summary_trigrams_separated_eng %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
summary_counts_eng <- summary_trigrams_filtered_eng %>%
  count(word1, word2, word3, sort = TRUE)
######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################
#install.packages("igraph")
library(igraph)
summary_trigram_graph_eng <- summary_counts_eng %>%
  filter(n>1) %>%
  graph_from_data_frame()

summary_trigram_graph_eng

#install.packages("ggraph")
library(ggraph)
ggraph(summary_trigram_graph_eng, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

# Description english -----------------------------------------------------
#let's continue with the description
#Let's work on the N Grams for each of one of them
description_quadrograms_eng <- english_description %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4)
#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
description_quadrograms_separated_eng <- description_quadrograms_eng %>%
  separate(quadrogram, c("word1", "word2","word3", "word4"), sep = " ")

description_quadrograms_filtered_eng <- description_quadrograms_separated_eng %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
description_counts_eng <- description_quadrograms_filtered_eng %>%
  count(word1, word2, word3, word4, sort = TRUE)
#Visual N.gram network
#install.packages("igraph")
library(igraph)
description_quadrogram_graph_eng <- description_counts_eng %>%
  filter(n>1) %>%
  graph_from_data_frame()

description_quadrogram_graph_eng

#install.packages("ggraph")
library(ggraph)
library(dplyr)

ggraph(description_quadrogram_graph_eng, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

# Correlograms ------------------------------------------------------------
#Correlelograms
library(tidyr)
library(tidytext)
library(stringr)
frequency_eng <- bind_rows(mutate(token_summary_eng, author ="Summary"),
                           mutate(token_description_eng, author = "Description")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `Description`)

#let's plot the correlograms:
library(scales)
library(plotly)
plotlycor_eng <-ggplot(frequency_eng, aes(x=proportion, y=`Summary`, 
                                          color = abs(`Summary`- proportion), key=word))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Summary", x=NULL)

plotlycor_eng <- ggplotly(plotlycor_eng, tooltip = c("key", "proportion"))



# TF -IDF english ---------------------------------------------------------
#TF_IDF
#we're grouping by the country this time
summary_token_eng <- english_summary %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  count(country, word, sort=TRUE) %>%
  ungroup()

total_words_eng <- summary_token_eng %>%
  group_by(country) %>%
  summarise(total=sum(n))

summary_words_eng <- left_join(summary_token_eng, total_words_eng)


print(summary_words_eng)

library(ggplot2)
ggplot(summary_words_eng, aes(n/total, fill = country))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.001) +
  facet_wrap(~country, ncol=2, scales="free_y")

country_words_summary_eng <- summary_words_eng %>%
  bind_tf_idf(word, country, n)

country_words_summary_eng # we get all the zeors because we are looking at stop words ... too common

country_words_summary_eng %>%
  arrange(desc(tf_idf))

#############
# looking at the graphical approach:
country_words_summary_eng %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(country) %>%
  top_n(5) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=country))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~country, ncol=2, scales="free")+
  coord_flip()

#same for description
description_token_eng <- english_description %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  count(country, word, sort=TRUE) %>%
  ungroup()

total_words_desc_eng <- description_token_eng %>%
  group_by(country) %>%
  summarise(total=sum(n))

description_words_eng <- left_join(description_token_eng, total_words_desc_eng)


print(description_words_eng)

library(ggplot2)
ggplot(description_words_eng, aes(n/total, fill = country))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.001) +
  facet_wrap(~country, ncol=2, scales="free_y")

country_words_description_eng <- description_words_eng %>%
  bind_tf_idf(word, country, n)

country_words_description_eng # we get all the zeors because we are looking at stop words ... too common

country_words_description_eng %>%
  arrange(desc(tf_idf))

#############
# looking at the graphical approach:
country_words_description_eng %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(country) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=country))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~country, ncol=2, scales="free")+
  coord_flip()


# english Notes -----------------------------------------------------------
#Question6
english_notes<- english_df$notes
english_notes <- data.frame(line=1:3959, text=english_notes)
english_notes <- cbind(english_notes, english_df$country)
col <- c("line", "text", "country")
colnames(english_notes) <- col

fdflanguagerules <- textcat(english_df$house_rules)
english_df$languagerules <- fdflanguagerules

english_houserules <- english_df %>% 
  filter(languagerules == "english"  | languagerules == "english" ) %>% 
  select(house_rules, country)
english_houserules <- data.frame(line=1:1994, text=english_houserules)

colnames(english_houserules) <- col


#We are going to tokenize
token_notes_eng <- english_notes %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

token_houserules_eng <- english_houserules %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE)

#from this tokenization we create a wordcloud
#Doing a wordcloud to see what is associated with sustainable fashion
#install.packages("wordcloud2")
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)

token_notes_100_eng <- token_notes_eng %>%
  top_n(100)

notes_wordcloud_eng <-wordcloud2(data = token_notes_100_eng, 
                                 size = 0.6, color = "random-dark")

token_houserules_100_eng <- token_houserules_eng %>%
  top_n(100)

houserules_wordcloud_eng <-wordcloud2(data = token_houserules_100_eng, 
                                      size = 0.6, color = "random-dark")

#Let's work on the N Grams for each of one of them
#Let's start with summary
notes_bigrams_eng <- english_notes %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
notes_bigrams_separated_eng <- notes_bigrams_eng %>%
  separate(bigram, c("word1", "word2"), sep = " ")

notes_bigrams_filtered_eng <- notes_bigrams_separated_eng %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) 

#creating the new bigram, "no-stop-words":
notes_counts_eng <- notes_bigrams_filtered_eng %>%
  count(word1, word2, sort = TRUE)
######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################
#install.packages("igraph")
library(igraph)
notes_bigram_graph_eng <- notes_counts_eng %>%
  filter(n>1) %>%
  graph_from_data_frame()

notes_bigram_graph_eng

#install.packages("ggraph")
library(ggraph)
ggraph(notes_bigram_graph_eng, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

#let's continue with the description
#Let's work on the N Grams for each of one of them
houserules_bigrams_eng <- english_houserules %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
houserules_bigrams_separated_eng <- houserules_bigrams_eng %>%
  separate(bigram, c("word1", "word2"), sep = " ")

houserules_bigrams_filtered_eng <- houserules_bigrams_separated_eng %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) 

#creating the new bigram, "no-stop-words":
houserules_counts_eng <- houserules_bigrams_filtered_eng %>%
  count(word1, word2, sort = TRUE)
######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################
#install.packages("igraph")
library(igraph)
houserules_bigram_graph_eng <- houserules_counts_eng %>%
  filter(n>1) %>%
  graph_from_data_frame()

houserules_bigram_graph_eng

#install.packages("ggraph")
library(ggraph)
library(dplyr)

ggraph(houserules_bigram_graph_eng, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

#Correlelograms
library(tidyr)
library(tidytext)
library(stringr)
frequency_notes_hr_eng <- bind_rows(mutate(token_notes_eng, author ="Notes"),
                                    mutate(token_houserules_eng, author = "House Rules")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `House Rules`)

#let's plot the correlograms:
library(scales)
library(plotly)
plotlycor_eng <-ggplot(frequency_notes_hr_eng, aes(x=proportion, y=`Notes`, 
                                                   color = abs(`Notes`- proportion), key=word))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Notes", x=NULL)

plotlycor2 <- ggplotly(plotlycor_eng, tooltip = c("key", "proportion"))


# TF_IDF ------------------------------------------------------------------
#TF_IDF
#we're grouping by the country this time
notes_token_eng <- english_notes %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  count(country, word, sort=TRUE) %>%
  ungroup()

total_words_notes_eng <- notes_token_eng %>%
  group_by(country) %>%
  summarise(total=sum(n))

notes_words_eng <- left_join(notes_token_eng, total_words_notes_eng)


print(notes_words_eng)

library(ggplot2)
ggplot(notes_words_eng, aes(n/total, fill = country))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.001) +
  facet_wrap(~country, ncol=2, scales="free_y")

country_words_notes_eng <- notes_words_eng %>%
  bind_tf_idf(word, country, n)

country_words_notes_eng # we get all the zeors because we are looking at stop words ... too common

country_words_notes_eng %>%
  arrange(desc(tf_idf))


# looking at the graphical apprach:
country_words_notes_eng %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(country) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=country))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~country, ncol=2, scales="free")+
  coord_flip()


####################### sentiment analysis
###Text to String ########
TextListToString<-c()
for (text in airbnb_all$host_verifications) {
  TextListToString<-c(TextListToString, toString(text))
}

airbnb_all$host_verifications<-TextListToString


airbnb_all$amenities

TextListToString1<-c()
for (text in airbnb_all$amenities) {
  TextListToString1<-c(TextListToString1, toString(text))
}

airbnb_all$amenities<-TextListToString1

######## Sentiment analysis interactions ( Superhost as filter) ####

#### Superhost "TRUE"####
host <- airbnb_all$host
use_airbnb <- cbind(airbnb_all, host)

Airbnb_interaction_super <- use_airbnb %>% 
  filter(host_is_superhost == TRUE) %>% 
  unnest_tokens(word, interaction) %>% 
  anti_join(stop_words) %>% 
  count( host_is_superhost,word, sort = T)


###BING
Airbnb_bing_super <- Airbnb_interaction_super %>% 
  inner_join(get_sentiments("bing")) %>%  
  count(sentiment,word, sort = TRUE) %>% 
  ungroup()

Airbnb_bing_super

####Afinn

afinn1_super <- Airbnb_interaction_super %>%
  inner_join(get_sentiments("afinn"))%>% #Bringing the afinn library
  summarise(sentiment=sum(value)) %>% #summarise the data
  mutate(method="AFINN")


#NRC
Airbnb_nrc_super <- Airbnb_interaction_super %>% 
  inner_join(get_sentiments("nrc")) %>%  
  count(sentiment,word, sort = TRUE)
Airbnb_nrc_super

#Plotting 
bing_and_nrc_super <- bind_rows(
  Airbnb_interaction_super%>%
    inner_join(get_sentiments("bing"))%>% #combine the tokenize india with Bing
    mutate(method = "Bing et al."),
  Airbnb_interaction_super %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn1_super, bing_and_nrc_super) %>% #how they interact with eachother in a graph
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")


###### Superhost "FALSE" #######

Airbnb_interaction_superF <- use_airbnb %>% 
  filter(host_is_superhost == FALSE) %>% 
  unnest_tokens(word, interaction) %>% 
  anti_join(stop_words) %>% 
  count( host_is_superhost,word, sort = T)


###BING
Airbnb_bing_superF <- Airbnb_interaction_superF %>% 
  inner_join(get_sentiments("bing")) %>%  
  count(sentiment,word, sort = TRUE) %>% 
  ungroup()

Airbnb_bing_superF

####Afinn

afinn1_superF <- Airbnb_interaction_superF %>%
  inner_join(get_sentiments("afinn"))%>% #Bringing the afinn library
  summarise(sentiment=sum(value)) %>% #summarise the data
  mutate(method="AFINN")
afinn1_superF

#NRC
Airbnb_nrc_superF <- Airbnb_interaction_superF %>% 
  inner_join(get_sentiments("nrc")) %>%  
  count(sentiment,word, sort = TRUE)
Airbnb_nrc_superF

#Plotting 
bing_and_nrc_superF <- bind_rows(
  Airbnb_interaction_superF%>%
    inner_join(get_sentiments("bing"))%>% #combine the tokenize india with Bing
    mutate(method = "Bing et al."),
  Airbnb_interaction_superF %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn1_superF, bing_and_nrc_superF) %>% #how they interact with eachother in a graph
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

###### N -grams ###### •	N-grams superhost & interactions

interaction_ready <- as.data.frame(use_airbnb$interaction)
colnames(interaction_ready) <- c("interaction")
superhost_ready <- as.data.frame(use_airbnb$host_is_superhost)
colnames(superhost_ready) <- c("superhost")

airbnb_ready <- cbind(interaction_ready, superhost_ready)

trigram_interaction <- airbnb_ready%>%
  unnest_tokens(trigram, interaction , token = "ngrams", n=3)

########separating words
trigrams_separated_interaction <- trigram_interaction %>%
  separate(trigram, c("word1", "word2","word3"), sep = " ")

#filtering out stop words
trigrams_filtered_interaction <- trigrams_separated_interaction%>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  na.omit()

#creating the new trigram, "no-stop-words":
trigram_counts_interaction<- trigrams_filtered_interaction %>%
  count(superhost, word1, word2,word3, sort = TRUE) %>% 
  group_by(superhost)


####### VISUALISING A TRIGRAM NETWORK #################
#install.packages("igraph")
library(igraph)

trigram_graph_interaction <- trigram_counts_interaction %>%
  filter(n>3) %>%
  graph_from_data_frame()

trigram_graph_interaction

#install.packages("ggraph")
a <- grid::arrow(type="open", length = unit(.10,"inches")) 
b <- grid::arrow(type="closed", length = unit(.08,"inches")) 
library(ggraph)
ggraph(trigram_graph_interaction, layout = "fr") + #"fr" means frequency
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = b)+ #including the arrow direction to the graph
  geom_node_point(color="#009999", size = 2)+ #changing color and size of the node http://www.sthda.com/english/wiki/colors-in-r  
  geom_node_text(aes(label=name), vjust =1, hjust=1) + #label or word in this case
  theme_gray()
  