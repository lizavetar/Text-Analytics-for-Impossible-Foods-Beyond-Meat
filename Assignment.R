
################################################
#                importing data                #
################################################

#---Getting Articles Data-----------------------


library(magrittr)
library(rvest)
library(textreadr)

# importing articles for Impossible Foods

setwd("/Users/lizavetaradzevich/Desktop/Lizaveta/HULT/DD/Text Analysis/Assignment/if")
nm <- list.files(path="/Users/lizavetaradzevich/Desktop/Lizaveta/HULT/DD/Text Analysis/Assignment/if")
if_articles <- do.call(rbind, lapply(nm, function(x) read_doc(file=x)))


# importing articles for Beyond Meat

setwd("/Users/lizavetaradzevich/Desktop/Lizaveta/HULT/DD/Text Analysis/Assignment/bm")
nm <- list.files(path="/Users/lizavetaradzevich/Desktop/Lizaveta/HULT/DD/Text Analysis/Assignment/bm")
bm_articles <- do.call(rbind, lapply(nm, function(x) read_doc(file=x)))


# importing articles for Both

setwd("/Users/lizavetaradzevich/Desktop/Lizaveta/HULT/DD/Text Analysis/Assignment/both")
nm <- list.files(path="/Users/lizavetaradzevich/Desktop/Lizaveta/HULT/DD/Text Analysis/Assignment/both")
both_articles <- do.call(rbind, lapply(nm, function(x) read_doc(file=x)))
#---Getting Twitter Data------------------------
# importing Twitter data from 2013-06-01

library("twitteR")
library("tm")
#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- 'qWbkEsTLeQrmhOJ8xWPX85DcN'
consumer_secret <- 'AaHbrZ3MokX6zAdGjv48vK7CTgA8FmSAwN1570Ppl3iKaJU29Z'
access_token <- '1218345975669854208-gy56ng6kkqsU5ENpol3DYDq3VPGGNC'
access_secret <- 'uDkpfmGMWCc2EmZKjCW8criBrKdS0ryCiBvVdIt9Z2CGV'

 setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# importing Twitter data for Beyond Meat 

get_bm_twitter <- twitteR::searchTwitter('#impossiblefoods ', n = 1000, since = '2013-06-01', retryOnRateLimit = 1e3)
bm_twitter = twitteR::twListToDF(get_bm_twitter)
write.csv(bm_twitter, file='/Users/lizavetaradzevich/Desktop/Lizaveta/HULT/DD/Text Analysis/Assignment/Twitter/bmtritter.csv')


# importing Twitter Data for Impossible Foods

get_if_twitter <- twitteR::searchTwitter('#beyondmeat', n = 1000, since = '2013-06-01', retryOnRateLimit = 1e3)
if_twitter = twitteR::twListToDF(get_if_twitter)
write.csv(if_twitter, file='/Users/lizavetaradzevich/Desktop/Lizaveta/HULT/DD/Text Analysis/Assignment/Twitter/iftritter.csv')

#---Getting Impact Report Data------------------
if_impact_report <- read_pdf('/Users/lizavetaradzevich/Desktop/Lizaveta/HULT/DD/Text Analysis/Assignment/im_Digital_Impact_Report_2019.pdf')
bm_impact_report <- read_pdf('/Users/lizavetaradzevich/Desktop/Lizaveta/HULT/DD/Text Analysis/Assignment/bm_Digital_Impact_Report_2019.pdf')





################################################
#             data preprocessing               #
################################################

#---Creating Empty Data Frames-------------------
# creating an empty data frame for Impossible Foods
if_df_articles <- data.frame(matrix(ncol = 2, nrow = 7))
col_names <- c("article", "text")
colnames(if_df_articles) <- col_names

# creating an empty data frame for Beyond Meat
bm_df_articles <- data.frame(matrix(ncol = 2, nrow = 9))
colnames(bm_df_articles) <- col_names

# creating an empty data frame for both
both_df_articles <- data.frame(matrix(ncol = 2, nrow = 9))
colnames(both_df_articles) <- col_names


#---Creating lists of acticles names--------------------------------------------
# creating a list of the names for Impossible Foods
articles_if <-c('Exclusive: Impossible Foods has stopped McDonald...',
                'Impossible Burger: Here’s what’s really in it ',
                'Impossible made fake meat a hot commodity',
                'Is the Impossible Burger Healthy?',
                'The Impossible Burger Could Change the Meat',
                'Unregulated Allergens in the Impossible Sausage',
                'What’s Different About the Impossible Burger? ')

# creating a list of the names for Beyond Meat
articles_bm <- c('Beyond Meat — Beyond a Trend?',
                 'Beyond Meat Burger It’s Vegan!',
                 'Beyond Meat Stock Is Falling',
                 'Beyond Meat tests plant-based fried chicken in KFC',
                 'Beyond Meat will soon be on the menu at 11 food chains',
                 'Beyond Meat, Beyond Growth',
                 'Future of Food Vol 3: Going Beyond Meat More',
                 'KFC’s Beyond Meat Chicken Is a Damn Miracle',
                 'KFC’s Beyond Meat Chicken Is a Damn Miracl',
                 'Why Companies Like Beyond Meat Are Important')

# creating a list of the names for both
articles_both <- c('CAN A BURGER HELP SOLVE CLIMATE CHANGE?',
                   'Falling in Love With Meat Again',
                   'Is Game Changers Funded by Impossible Foods or Beyond Meat',
                   'Meatless meat is becoming mainstream',
                   'Plant-Based Meat’ Is All Hat and No Cattle',
                   'The Fake Backlash to Fake Meat',
                   'This Is the Beginning of the End of the Beef Industry ',
                   'What’s in This?: Veggie Burgers',
                   'Why Beyond Meat + Impossible are bad for you')

#---Filling in articles data frames--------------------------------------------
# fillin gin the data frame with name of the article and content for Impossible Foods

for (i in 1:7) {
  if_df_articles[i, 1] <- articles_if[i]
}

for (j in 1:7) {
  if_df_articles[j, 2] <- if_articles[j]
}


# fillin gin the data frame with name of the article and content for Beyond Meat
for (i in 1:9) {
  bm_df_articles[i, 1] <- articles_bm[i]
}

for (j in 1:9) {
  bm_df_articles[j, 2] <- bm_articles[j]
}

# fillin gin the data frame with name of the article and content for both 
for (i in 1:9) {
  both_df_articles[i, 1] <- articles_both[i]
}

for (j in 1:9) {
  both_df_articles[j, 2] <- both_articles[j]
}





################################################
#                tokenization                  #
################################################

#---Custom stop-words--------------------------------------------
cust_stop_words <- data.frame(word = c('impossiblefoods', 'rt', 'impossible', 'impossiblesausage',
                                       'croissan’wi', 'https', 't.co', 'beyondmeat', 'meat', '6597712380',
                                       'dex', 'ng', 'bynd', 'foods', 'plant', 'based', 'food', 'burger', 
                                       'burgers', 'they', 'you', 'we', 'i', 'lca', '1'), 
                                        lexicon = rep('CUST', each = 25))

#---Tokenizing Twitter Data-----------
# tokenizing bm_twitter data
library(dplyr)
library(tidytext)

token_bm_twitter <- bm_twitter %>%
  unnest_tokens(word, text)  %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop_words) %>%
  count(word, sort = T) # sort by TRUE gives most frequent at the top



# tokenizing if_twitter data
token_if_twitter <- if_twitter %>%
  unnest_tokens(word, text)  %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop_words) %>%
  count(word, sort = T) # sort by TRUE gives most frequent at the top

#---Tokenizing articles Data--------------------------------------------

# tokenizing Impossible Food articles data
token_if_articles <- if_df_articles %>%
  unnest_tokens(word, text)  %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop_words) %>%
  count(word, sort = T) # sort by TRUE gives most frequent at the top

# tokenizing Beyond Meat acticles data
token_bm_articles <- bm_df_articles %>%
  unnest_tokens(word, text)  %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop_words) %>%
  count(word, sort = T) # sort by TRUE gives most frequent at the top

# tokenizing both companies acticles data
token_both_articles <- both_df_articles %>%
  unnest_tokens(word, text)  %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop_words) %>%
  count(word, sort = T) # sort by TRUE gives most frequent at the top


#---Tokenizing Impact Report Data-------
token_if_impact_report <- if_impact_report %>%
  unnest_tokens(word, text)  %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop_words) %>%
  count(word, sort = T) # sort by TRUE gives most frequent at the top

token_bm_impact_report <- bm_impact_report %>%
  unnest_tokens(word, text)  %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop_words) %>%
  count(word, sort = T) # sort by TRUE gives most frequent at the top




################################################
#             sentiment analysis               #
################################################

#---sentiment of articles-----------------------
library(reshape2)
library(wordcloud)

afinn <- get_sentiments('afinn')
nrc <- get_sentiments('nrc')
bing <- get_sentiments('bing')

sentiments <- bind_rows(mutate(afinn, lexicon = 'afinn'),
                        mutate(nrc, lexicon = 'nrc'),
                        mutate(bing, lexicon = 'bing'))

# getting sentiment analysis visualization for Impossible Foods
token_if_articles %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")


token_if_articles %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, 
                   scale = c(0.5, 0.5),
                   fixed.asp = TRUE,
                   title.size = 1)

token_if_articles %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, 
                   scale = c(0.7, 0.7),
                   fixed.asp = TRUE,
                   title.size = 1)


# getting sentiment analysis visualization for Beyond Meat
token_bm_articles %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

token_bm_articles %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, 
                   scale = c(0.5, 0.5),
                   fixed.asp = TRUE,
                   title.size = 1)

token_bm_articles %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, 
                   scale = c(0.7, 0.7),
                   fixed.asp = TRUE,
                   title.size = 1)

# getting sentiment analysis visualization for both

token_both_articles %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")


token_both_articles %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, 
                   scale = c(0.5, 0.5),
                   fixed.asp = TRUE,
                   title.size = 1)

token_both_articles %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, 
                   scale = c(0.7, 0.7),
                   fixed.asp = TRUE,
                   title.size = 1)


#---sentiment of twitter data-----
token_if_twitter %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")


token_if_twitter %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, 
                   scale = c(0.7, 0.7),
                   fixed.asp = TRUE,
                   title.size = 1)

token_if_twitter %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, 
                   scale = c(0.7, 0.7),
                   fixed.asp = TRUE,
                   title.size = 1)

token_bm_twitter %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")


token_bm_twitter %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, 
                   scale = c(0.7, 0.7),
                   fixed.asp = TRUE,
                   title.size = 1)

token_bm_twitter %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, 
                   scale = c(0.7, 0.7),
                   fixed.asp = TRUE,
                   title.size = 1)

#---sentiment of impact reports ----

token_if_impact_report %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

token_if_impact_report %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, 
                   scale = c(0.7, 0.7),
                   fixed.asp = TRUE,
                   title.size = 1)

token_if_impact_report %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, 
                   scale = c(0.7, 0.7),
                   fixed.asp = TRUE,
                   title.size = 1)


token_bm_impact_report %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

token_bm_impact_report %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, 
                   scale = c(0.7, 0.7),
                   fixed.asp = TRUE,
                   title.size = 1)

token_bm_impact_report %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, 
                   scale = c(0.7, 0.7),
                   fixed.asp = TRUE,
                   title.size = 1)



################################################
#                 correlations                 #
################################################
#---articles correlogram-------
library(tidyr)
library(stringr)
library(ggplot2)
#########################
# double try

articles_try <- bind_rows(mutate(token_if_articles, company="Impossible_Foods"),
                          mutate(token_bm_articles, company= "Beyond_Meat")
                          )

frequency <- articles_try %>% 
             group_by(company) %>% 
             count(word, sort = TRUE) %>% 
             left_join(articles_try %>%
                       group_by(company) %>%
                       summarise(total = n())) %>%
             mutate(freq = n/total)

frequency <- frequency %>%
  select(company, word, freq) %>%
  spread(company, freq) %>%
  arrange(Beyond_Meat, Impossible_Foods)

ggplot(frequency, aes(Beyond_Meat, Impossible_Foods)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) + 
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) + 
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")

#########################

articles_correlation <- bind_rows(mutate(token_if_articles, company="Impossible_Foods"),
                                  mutate(token_bm_articles, company= "Beyond_Meat"),
                                  mutate(token_both_articles, company="Both")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(company, word) %>%
  anti_join(cust_stop_words) %>%
  group_by(company) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(company, proportion) %>%
  gather(company, proportion, `Impossible_Foods`, `Beyond_Meat`)

#let's plot the correlograms:
library(scales)
articles_correlogram <- ggplot(articles_correlation, aes(x=proportion, y=`Both`, 
                               color = abs(`Both`- proportion)))

articles_correlogram <- articles_correlogram + geom_abline(color="grey40", lty=2)+
          geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
          geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
          scale_x_log10(labels = percent_format())+
          scale_y_log10(labels= percent_format())+
          scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
          facet_wrap(~company, ncol=2)+
          theme(legend.position = "none")+
          labs(y= "Both", x=NULL)

#---twitter correlogram-------

twitter_correlation <- bind_rows(mutate(token_if_twitter, company="Impossible_Foods"),
                                 mutate(token_bm_twitter, company= "Beyond_Meat")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(company, word) %>%
  anti_join(cust_stop_words) %>%
  group_by(company) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(company, proportion) %>%
  gather(company, proportion, `Impossible_Foods`)

#let's plot the correlograms:
twitter_correlogram  <- ggplot(twitter_correlation, aes(x=proportion, y=`Beyond_Meat`, 
                                color = abs(`Beyond_Meat`- proportion)))

twitter_correlogram <- twitter_correlogram + geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~company, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Beyond_Meat", x=NULL)

#---impact report correlogram-------
impact_report_correlation <- bind_rows(mutate(token_if_impact_report, company="Impossible_Foods"),
                                 mutate(token_bm_impact_report, company= "Beyond_Meat")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(company, word) %>%
  anti_join(cust_stop_words) %>%
  group_by(company) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(company, proportion) %>%
  gather(company, proportion, `Impossible_Foods`)

#let's plot the correlograms:
impact_report_correlogram  <- ggplot(impact_report_correlation, aes(x=proportion, y=`Beyond_Meat`, 
                                                        color = abs(`Beyond_Meat`- proportion)))

impact_report_correlogram <- impact_report_correlogram + geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~company, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Beyond_Meat", x=NULL)



################################################
#            inverse frequencies               #
################################################
#---Articles-------
# Which words are showing that we are talking about Impossible Foods or Beyond Meat

if_df_articles['company'] <- 'Impossible Foods'
bm_df_articles['company'] <- 'Beyond Meat'
both_df_articles['company'] <- 'Both'

# creating one data frame with all articles
articles <- bind_rows(if_df_articles, bm_df_articles, both_df_articles)

# tokenizing new data frame
original_articles <- articles %>%
  unnest_tokens(word, text) %>%
  count(company, word, sort=TRUE) %>%
  ungroup()

# counting total number of tokens by company
total_words_articles <- original_articles %>%
  group_by(company) %>%
  summarize(total=sum(n))

# goinging tokens and word count
articles_words <- left_join(original_articles, total_words_articles)

#assignin gfrequency ranks
freq_by_rank <- articles_words %>%
  group_by(company) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)

articles_words <- articles_words %>%
  bind_tf_idf(word, company, n)

articles_words %>%
  arrange(desc(tf_idf))


# looking at the graphical apprach:
# which words are spesific for each company?
articles_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(company) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=company))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~company, ncol=1, scales="free")+
  coord_flip()

#---Twitter-----
if_twitter['company'] <- 'Impossible Foods'
bm_twitter['company'] <- 'Beyond Meat'

twitter <- bind_rows(if_twitter, bm_twitter)

# tokenizing new data frame
original_twitter <- twitter %>%
  unnest_tokens(word, text) %>%
  count(company, word, sort=TRUE) %>%
  ungroup()

# counting total number of tokens by company
total_words_twitter <- original_twitter %>%
  group_by(company) %>%
  summarize(total=sum(n))

# goinging tokens and word count
twitter_words <- left_join(original_twitter, total_words_twitter)

#assignin gfrequency ranks
freq_by_rank <- twitter_words %>%
  group_by(company) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)

twitter_words <- twitter_words %>%
  bind_tf_idf(word, company, n)

twitter_words %>%
  arrange(desc(tf_idf))


# looking at the graphical apprach:
# which words are spesific for each company?
twitter_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(company) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=company))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~company, ncol=1, scales="free")+
  coord_flip()

#---Impact Reports----

if_impact_report['company'] <- 'Impossible Foods'
bm_impact_report['company'] <- 'Beyond Meat'

impact_report <- bind_rows(if_impact_report, bm_impact_report)

# tokenizing new data frame
original_impact_report <- impact_report %>%
  unnest_tokens(word, text) %>%
  count(company, word, sort=TRUE) %>%
  ungroup()

# counting total number of tokens by company
total_words_impact_report <- original_impact_report %>%
  group_by(company) %>%
  summarize(total=sum(n))

# goinging tokens and word count
impact_report_words <- left_join(original_impact_report, total_words_impact_report)

#assignin gfrequency ranks
freq_by_rank <- impact_report_words %>%
  group_by(company) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)

impact_report_words <- impact_report_words %>%
  bind_tf_idf(word, company, n)

impact_report_words %>%
  arrange(desc(tf_idf))


# looking at the graphical apprach:
# which words are spesific for each company?
impact_report_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(company) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=company))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~company, ncol=1, scales="free")+
  coord_flip()



################################################
#                   n-grams                    #
################################################

#---Articles------
actricles <- articles[c('company', 'text')]

bigram_articles <- articles %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) 

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
bigrams_articles_sep <- bigram_articles %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigram_articles_filtered <- bigrams_articles_sep %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) 


#creating the new bigram, "no-stop-words":
bigram_articles_counts <- bigram_articles_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams


bigram_articles_united <- bigram_articles_filtered %>%
  unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section

bigram_articles_tf_idf <- bigram_articles_united %>%
  count(company, bigram) %>%
  bind_tf_idf(bigram, company, n) %>%
  arrange(desc(tf_idf))

# visualizing bigram network
library(igraph)
bigram_graph <- bigram_articles_counts %>%
  filter(n>20) %>%
  graph_from_data_frame()

bigram_graph

library(ggraph)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_edge_link(color = "lightblue", size = 5)+
  geom_node_point(color = "lightblue", size = 5)+
  geom_node_text(aes(label=name), vjust =1, hjust=1)


#---Twitter------

twitter <- twitter[c('company', 'text')]

bigram_twitter <- twitter %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) 

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
bigrams_twitter_sep <- bigram_twitter %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigram_twitter_filtered <- bigrams_twitter_sep %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) 


#creating the new bigram, "no-stop-words":
bigram_twitter_counts <- bigram_twitter_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams


bigram_twitter_united <- bigram_twitter_filtered %>%
  unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section

bigram_twitter_tf_idf <- bigram_twitter_united %>%
  count(company, bigram) %>%
  bind_tf_idf(bigram, company, n) %>%
  arrange(desc(tf_idf))

# visualizing bigram network
bigram_graph <- bigram_twitter_counts %>%
  filter(n>20) %>%
  graph_from_data_frame()

bigram_graph

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_edge_link(color = "lightblue", size = 5)+
  geom_node_point(color = "lightblue", size = 5)+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

#---Impact Report----

impact_report <- impact_report[c('company', 'text')]

bigram_impact_report <- impact_report %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) 

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
bigrams_impact_report_sep <- bigram_impact_report %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigram_impact_report_filtered <- bigrams_impact_report_sep %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) 


#creating the new bigram, "no-stop-words":
bigram_impact_report_counts <- bigram_impact_report_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams


bigram_impact_report_united <- bigram_impact_report_filtered %>%
  unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section

bigram_impact_report_tf_idf <- bigram_impact_report_united %>%
  count(company, bigram) %>%
  bind_tf_idf(bigram, company, n) %>%
  arrange(desc(tf_idf))

# visualizing bigram network
bigram_graph <- bigram_impact_report_counts %>%
  filter(n>20) %>%
  graph_from_data_frame()

bigram_graph

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_edge_link(color = "lightblue", size = 5)+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)



################################################
#               negation tokens                #
################################################

#--- Articles------
negation_tokens <- c("no", "never", "without", "not") #what negation tokens do you want to use?
afinn_data = get_sentiments("afinn")

negated_words <- bigrams_articles_sep %>%
  filter(word1 %in% negation_tokens) %>%
  inner_join(afinn_data, by=c(word2="word")) %>%
  count(word1, word2, value, sort=TRUE) %>%
  ungroup()

negated_words

negated_words_plot <- function(x){
  negated_words %>%
    filter(word1 == x) %>%
    mutate(contribution = n* value) %>%
    arrange(desc(abs(contribution))) %>%
    head(20) %>%
    mutate(word2 = reorder(word2, contribution)) %>%
    ggplot(aes(word2, n*value, fill = n*value >0))+
    geom_col(show.legend = FALSE)+
    xlab(paste("Words preceded by", x))+
    ylab("Sentiment score* number of occurences")+
    coord_flip()
}#closing the negated_words_plot function

negated_words_plot(x="no") #this is your first negation word
negated_words_plot(x="never") #this is your second negation word
negated_words_plot(x="not") #this is your third negation word

#---Twitter------

negated_words <- bigrams_twitter_sep %>%
  filter(word1 %in% negation_tokens) %>%
  inner_join(afinn_data, by=c(word2="word")) %>%
  count(word1, word2, value, sort=TRUE) %>%
  ungroup()

negated_words
negated_words_plot(x="not") #this is your third negation word

#---Impact Articles----

negated_words <- bigrams_impact_report_sep %>%
  filter(word1 %in% negation_tokens) %>%
  inner_join(afinn_data, by=c(word2="word")) %>%
  count(word1, word2, value, sort=TRUE) %>%
  ungroup()

negated_words

negated_words_plot(x="no") #this is your first negation word
negated_words_plot(x="not") #this is your third negation word


