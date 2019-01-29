## load libraries 
library(tidyverse)
library(scales)
library(forcats)
library(tidytext)

## example 
sentences <- data_frame(sentence = 1:3,
                        text = c("I just love this burger. It is amazing. America has the best burgers. I just love them. They are great. Someone says otherwise,  they are a loser",
                                 "This burger was terrible - bad taste all around. But I did like the music in the bar.",
                                 "I had a burger yesterday - it was ok. I ate all of it"))

tidy.sentences <- sentences %>%
  unnest_tokens(word,text)


## join sentiment lexicon
tidy.sentences %>%
  inner_join(get_sentiments("bing"),by="word")  ##  get_sentiments('nrc') 更多的情绪  

tidy.sentences %>%
  count(sentence,word) %>%
  inner_join(get_sentiments("bing"),by="word") 


tidy.sentences %>%
  count(sentence,word) %>%
  inner_join(get_sentiments("bing"),by="word") %>%
  group_by(sentence,sentiment) %>%
  summarize(total=sum(n))


## net sentiment 
tidy.sentences %>%
  count(sentence,word) %>%
  inner_join(get_sentiments("bing"),by="word") %>%
  group_by(sentence,sentiment) %>%
  summarize(total=sum(n)) %>%
  spread(sentiment,total) %>%
  mutate(net.positve=positive-negative)


## convention speeches
speech <- read_rds('data/convention_speeches.rds')

speech %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  count(speaker,word) %>%
  inner_join(get_sentiments('nrc')) %>%
  inner_join(sentiment.orientation,by='sentiment') %>%
  group_by(speaker,orientation,sentiment) %>%
  summarise(nn=n()/sum(n)) %>%
  ggplot(aes(x=fct_reorder(sentiment,nn),y=nn,group=speaker,fill=speaker)) + 
  geom_bar(stat = 'identity',position = 'dodge') +
  facet_wrap(~orientation,scales='free') %>%
  scale_y_continuous(labels=dplyr::percent)




tidy.speech <- speech %>%
  unnest_tokens(word,text) 

total.terms.speaker <- tidy.speech %>%
  count(speaker)

## plot sentiments
sentiment.orientation <- data.frame(orientation = c(rep("Positive",5),rep("Negative",5)),
                                    sentiment = c("anticipation","joy","positive","trust","surprise","anger","disgust","fear","negative","sadness"))

tidy.speech %>%
  count(speaker,word) %>%
  inner_join(get_sentiments("nrc"),by=c("word")) %>%
  group_by(speaker,sentiment) %>%
  summarize(total=sum(n)) %>%
  inner_join(total.terms.speaker) %>%
  mutate(relative.sentiment=total/n) %>%
  inner_join(sentiment.orientation) %>%
  ggplot(aes(x=sentiment,y=relative.sentiment,fill=speaker)) + geom_bar(stat='identity',position='dodge') + 
  facet_wrap(~orientation,scales='free')+
  ylab('Relative Sentiment')+
  labs(title='Sentiment of Convention Acceptance Speeches',
       subtitle='Hillary Clinton vs. Donald Trump')+
  scale_y_continuous(labels=percent)

## Las Vegas sentiments 

reviews <- read_rds('data/reviewsTripAll.rds')

meta.data <- reviews %>%
  select(hotel,reviewID,reviewRating)

reviewsTidy <- reviews %>%
  unnest_tokens(word,reviewText) %>%
  count(reviewID,word)

term.hotel <- reviewsTidy %>%
  inner_join(meta.data,by='reviewID') %>%
  group_by(hotel) %>%
  summarize(n.hotel=sum(n)) 

## sentiments by hotel 

bing <- get_sentiments("bing") 

hotel.sentiment <- reviewsTidy %>%
  inner_join(bing,by=c("word")) %>%
  left_join(meta.data,by='reviewID') %>%
  group_by(hotel,sentiment) %>%
  summarize(total=sum(n)) %>%
  inner_join(term.hotel,by='hotel') %>%
  mutate(relative.sentiment = total/n.hotel)


hotel.sentiment %>%
  ggplot(aes(x=sentiment,y=relative.sentiment,fill=sentiment)) + geom_bar(stat='identity') + 
  facet_wrap(~hotel)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))


## plot of net sentiment
hotel.sentiment %>%
  select(sentiment,relative.sentiment,hotel) %>%
  spread(sentiment,relative.sentiment) %>%
  mutate(net.pos = positive-negative) %>%
  ggplot(aes(x=fct_reorder(hotel,net.pos),y=net.pos)) + geom_point(size=4) + coord_flip()+
  scale_y_continuous(labels=percent)+ylab('Net Positive Sentiment')+xlab('Resort')


## wider range of sentiments
nrc <- get_sentiments("nrc")

hotel.sentiment <- reviewsTidy %>%
  inner_join(nrc,by=c("word")) %>%
  left_join(meta.data,,by='reviewID') %>%
  group_by(hotel,sentiment) %>%
  summarize(total=sum(n)) %>%
  inner_join(term.hotel,by='hotel') %>%
  mutate(relative.sentiment = total/n.hotel)


hotel.sentiment %>%
  ggplot(aes(x=sentiment,y=relative.sentiment,fill=sentiment)) + geom_bar(stat='identity') + 
  facet_wrap(~hotel,ncol=3)+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(legend.position="none")







