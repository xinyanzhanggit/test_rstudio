## load libraries 
library(tidyverse)
library(scales)
library(forcats)
library(wordcloud)
library(tidytext)
library(lubridate)
library(tm)

#install.packages('wordcloud')
#install.packages('tidytext')
#install.packages('tm')

## load review data for las vegas hotels 
load('data/vegas_hotels.rda')

## star ratings for each hotel 

reviews %>%
  left_join(select(business,business_id,name),
            by='business_id') %>%
  group_by(name) %>%
  summarize(n = n(),
            mean.star = mean(as.numeric(stars))) %>%
  arrange(desc(mean.star)) %>%
  ggplot() + 
  geom_point(aes(x=reorder(name,mean.star),y=mean.star,size=n))+
  coord_flip() +
  ylab('Mean Star Rating (1-5)') + 
  xlab('Hotel')



## example text 
example <- data.frame(doc_id=c(1:4),
                      text=c("I have a brown dog. My dog loves walks.",
                             "My dog likes food.",
                             "I like food.",
                             "Some dogs are black."),stringsAsFactors = F)
example

## create a document-term matrix in tidy format  
exampleTidy <- example %>%
  unnest_tokens(word,text)

dtmTidy <- exampleTidy %>%
  count(doc_id,word)

## removing stop words 

exampleTidyNoStop <- exampleTidy %>%
  anti_join(stop_words)


## get reviews for Aria Hotel
aria.id <-  filter(business, 
                   name=='Aria Hotel & Casino')$business_id
aria.reviews <- filter(reviews, 
                       business_id==aria.id)

## doc-term matrix - tidy 

AriaTidy <- aria.reviews %>%
  select(review_id,text,stars) %>%
  unnest_tokens(word,text)


AriaFreqWords <- AriaTidy %>%
  count(word) 


## plot top words

AriaFreqWords %>%
  top_n(25) %>%
  ggplot(aes(x=fct_reorder(word,n),y=n)) + geom_bar(stat='identity') + 
  coord_flip() + theme_bw()+
  labs(title='Top 25 Words in Aria Reviews',
       x='Count',
       y= 'Word')


## plot top words - stop words removed 

AriaFreqWords %>%
  anti_join(stop_words) %>%
  top_n(25) %>%
  ggplot(aes(x=fct_reorder(word,n),y=n)) + geom_bar(stat='identity') + 
  coord_flip() + theme_bw()+
  labs(title='Top 25 Words in Aria Reviews',
       subtitle = 'Stop Words Removed',
       x='Count',
       y= 'Word')


## Top words by rating 

AriaFreqWordsByRating <- AriaTidy %>%
  count(stars,word)



## for plotting (from https://github.com/dgrtwo/drlib/blob/master/R/reorder_within.R)
reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}


scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}


AriaFreqWordsByRating %>%
  anti_join(stop_words) %>%
  group_by(stars) %>%
  top_n(20) %>%
  ggplot(aes(x=reorder_within(word,n,stars),
             y=n,
             fill=stars)) + 
  geom_bar(stat='identity') + 
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~stars,scales = 'free',nrow=1) + 
  theme_bw() + 
  theme(legend.position = "none")+
  labs(title = 'Top Words by Review Rating',
       subtitle = 'Stop words removed',
       x = 'Word',
       y = 'Count')



## word clouds - visualizing a dtm 

topWords <- AriaFreqWords %>%
  anti_join(stop_words) %>%
  top_n(100) 
  
wordcloud(topWords$word,
           topWords$n,
           scale=c(5,0.5), 
           colors=brewer.pal(8,"Dark2"))


## repeat with bi-grams

aria.reviews %>%
  select(review_id,text) %>%
  unnest_tokens(bigram,text,token="ngrams",n=2) %>%
  count(bigram) %>%
  top_n(40) %>%
  ggplot(aes(x=fct_reorder(bigram,n),y=n)) + geom_bar(stat='identity') + 
  coord_flip() 

## what is a review really about? TF-IDF

tidyReviews <- aria.reviews %>%
  select(review_id,text) %>%
  unnest_tokens(word, text) %>%
  count(review_id,word)

minLength <- 200  # focus on long reviews 
tidyReviewsLong <- tidyReviews %>%
  group_by(review_id) %>%
  summarize(length = sum(n)) %>%
  filter(length >= minLength)

tidyReviews %>%
  filter(review_id %in% tidyReviewsLong$review_id) %>%
  bind_tf_idf(word,review_id,n) %>%
  group_by(review_id) %>%
  arrange(desc(tf_idf)) %>%
  slice(1:10) %>%
  ungroup() %>%
  mutate(xOrder=n():1)

?ungroup

tidyReviewsTFIDF <- tidyReviews %>%
  filter(review_id %in% tidyReviewsLong$review_id) %>%
  bind_tf_idf(word,review_id,n) %>%
  group_by(review_id) %>%
  arrange(desc(tf_idf)) %>%
  slice(1:10) %>% # get top 10 words in terms of tf-idf
  ungroup() %>%
  mutate(xOrder=n():1) %>%  # for plotting
  inner_join(select(aria.reviews,review_id,stars),by='review_id')  # get star ratings


nReviewPlot <- 12
plot.df <- tidyReviewsTFIDF %>%
  filter(review_id %in% tidyReviewsLong$review_id[1:nReviewPlot])

plot.df %>%
  mutate(review_id_n = as.integer(review_id)) %>%
  ggplot(aes(x=xOrder,y=tf_idf,fill=factor(review_id_n))) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ review_id_n,scales='free') +
  scale_x_continuous(breaks = plot.df$xOrder,
                     labels = plot.df$word,
                     expand = c(0,0)) + 
  coord_flip()+
  labs(x='Word',
       y='TF-IDF',
       title = 'Top TF-IDF Words in Reviews of Aria',
       subtitle = paste0('Based on first ', 
                         nReviewPlot,
                         ' reviews'))+
  theme(legend.position = "none")



## Aria on Tripadvisor reviews 
aria <- read_rds('data/AriaReviewsTrip.rds') %>%
  rename(text = reviewText)

meta.data <- aria %>%
  select(reviewID,reviewRating,date,year.month.group)

ariaTidy <- aria %>%
  select(reviewID,text) %>%
  unnest_tokens(word,text) %>%
  count(reviewID,word) %>%
  inner_join(meta.data,by="reviewID")

## word frequency over time 

ariaTidy %>%
  group_by(year.month.group) %>%
  summarise(n.total=sum(n))

total.terms.time <- ariaTidy %>%
  group_by(year.month.group) %>%
  summarize(n.total=sum(n))

## for the legend 
a <- 1:nrow(total.terms.time)
b <- a[seq(1, length(a), 3)]

三个不同的word在年月之间的变化 frequency的变化


ariaTidy %>%
  filter(word %in% c("pool","staff","buffet")) %>%
  group_by(year.month.group,word) %>%
  summarise(n=sum(n)) %>%
  inner_join(total.terms.time) %>%
  mutate(frequency = n/n.total) %>%
  ggplot(aes(x=year.month.group,y=frequency,group=word,color=word))+
  geom_line()+
  scale_x_discrete(breaks=as.character(total.terms.time$year.month.group[b])) + 
  scale_y_continuous(labels = percent)+
  facet_wrap(~word) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_flip()


ariaTidy %>%
  filter(word %in% c("pool","staff","buffet")) %>%
  group_by(word,year.month.group) %>%
  summarize(n = sum(n)) %>%
  left_join(total.terms.time, by='year.month.group') %>%
  ggplot(aes(x=year.month.group,y=n/n.total,color=word,group=word)) + 
  geom_line() + 
  facet_wrap(~word)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_discrete(breaks=as.character(total.terms.time$year.month.group[b]))+
  scale_y_continuous(labels=percent)+xlab('Year/Month')+
  ylab('Word Frequency relative to Month Total')+
  ggtitle('Dynamics of Word Frequency for Aria Hotel')


## same but for different satisfaction segments 
aria.tidy2 <- ariaTidy %>%
  mutate(year = year(date),
         satisfaction = fct_recode(factor(reviewRating),
                                   "Not Satisfied"="1",
                                   "Not Satisfied"="2",
                                   "Neutral"="3",
                                   "Neutral"="4",
                                   "Satisfied"="5"))

total.terms.rating.year <- aria.tidy2 %>%
  group_by(satisfaction,year) %>%
  summarize(n.total = sum(n)) 


aria.tidy2 %>%
  filter(word %in% c("pool","staff","buffet","food","wait","casino","line","check","clean")) %>%
  group_by(satisfaction,year,word) %>%
  summarize(n = sum(n)) %>%
  left_join(total.terms.rating.year, by=c('year','satisfaction')) %>%
  ggplot(aes(x=year,y=n/n.total,color=satisfaction,group=satisfaction)) + 
  geom_line(size=1,alpha=0.25) + geom_point() + 
  facet_wrap(~word,scales='free')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_continuous(labels=percent)+xlab('Year')+
  ylab('Word Frequency relative to Month Total')+
  labs(title='Dynamics of Word Frequency for Aria Hotel',
       subtitle='Three Satisfaction Segments')



