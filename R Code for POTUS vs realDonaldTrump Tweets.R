
path<-file.path(...,"realPOTUS.csv")
pres<-read.csv(path)

# Make sure the dates and times the tweets were created are in the 
# right format.
created<-as.character(pres$time)
created<-t(as.data.frame(strsplit(created,' ')))
row.names(created)=NULL
pres<-cbind(pres,created)
pres$Date<-pres$`1`
pres$Time<-pres$`2`
pres$Date<-as.Date(pres$Date,format='%d/%m/%Y')

# 'Salience of Twitter Accounts over Time
library(ggplot2)
windows()
ggplot(pres, aes(Date,user_followers_count,color=from_user)) + geom_line(size=.5) +
  theme_classic() + scale_color_manual(values=c("blue","red")) +
  ggtitle("Number of Followers",
          subtitle = "Start Date:  February 16, 2017\nEnd Date:  May 12, 2017") +
  xlab("Date") + ylab("Number of Followers") +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  theme(text=element_text(family="serif")) +
  scale_x_date(date_breaks = "1 week") +
  scale_y_continuous(labels = scales::comma, breaks=seq(0,27500000,2500000)) +
  theme(axis.text.x=element_text(angle=90,color="black")) +
  theme(legend.position=c(.8,.5)) +
  theme(legend.title = element_blank()) +
  theme(legend.background = element_rect(color="black", size=.5)) +
  theme(panel.grid.major.y=element_line(colour="lightgrey",linetype=1))

# Tweets by @POTUS and @realDonaldTrump
library(ggplot2)
library(plyr)
count<-count(pres,c("Date","from_user"))
windows()
ggplot(count, aes(Date,freq,color=from_user)) + geom_line(size=1) + theme_classic() +
  scale_color_manual(values=c("blue","red")) +
  ggtitle("Tweets by @POTUS and @RealDonaldTrump",
          subtitle="Start Date:  February 16, 2017\nEnd Date:  May 12, 2017") +
  xlab("Date Tweeted") + ylab("Total Number of Tweets") +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  theme(text=element_text(family="serif")) +
  scale_x_date(date_breaks = "1 week") +
  theme(axis.text.x=element_text(angle=90,color="black")) +
  theme(legend.position=c(.8,.9)) +
  theme(legend.title = element_blank()) +
  theme(legend.background = element_rect(color="black", size=.5)) +
  theme(panel.grid.major.y=element_line(colour="lightgrey",linetype=1))


# Sentiment Analysis of @POTUS vs @realDonaldTrump
library(syuzhet)
pres$text<-as.character(pres$text)
sent<-get_nrc_sentiment(pres$text)
sent$sentiment<-sent$positive-sent$negative
pres<-cbind(pres,sent)

windows()
ggplot(pres,aes(Date,sentiment,color=from_user)) +
  geom_point(alpha=.5) + geom_jitter(width=.25) +
  theme_classic() +
  scale_color_manual(values=c("blue","red")) +
  ggtitle("Sentiment of @POTUS vs. @realDonaldTrump Tweets") +
  theme(plot.subtitle=element_text(face="italic",size=14)) +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  scale_x_date(date_breaks = "1 week") +
  theme(axis.text.x=element_text(angle=90,color="black")) +
  ylab("<- More Negative...More Positive ->") + xlab("Date Tweeted") +
  theme(text=element_text(family="serif")) +
  theme(legend.position=c(.15,.9)) +
  theme(legend.title=element_blank()) +
  theme(legend.background = element_rect(color="black", size=.5)) +
  theme(panel.grid.major.y=element_line(colour="lightgrey",linetype=1))

windows()
note<-"POTUS Mean = 0.60\nrealDonaldTrump Mean = 0.47\nt = 1.2\np = 0.23"
ggplot(pres,aes(from_user,sentiment)) + coord_flip() +
  geom_boxplot() + geom_jitter(width=.1) + geom_point(alpha=I(.5)) +
  theme_classic() +
  ggtitle("Sentiment of @POTUS vs.\n@realDonaldTrump Tweets") +
  theme(plot.subtitle=element_text(face="italic",size=14)) +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  ylab("<- More Negative...More Positive ->") + xlab("") +
  theme(text=element_text(family="serif")) +
  theme(legend.background = element_rect(color="black", size=.5)) +
  geom_label(aes(x=2.39, y=2, label=note),hjust=0,family="serif",size=3) +
  theme(panel.grid.major.x=element_line(colour="lightgrey",linetype=1))
#Welch Two Sample t-test

#data:  sentiment by from_user
#t = 1.2011, df = 860.28, p-value =
#  0.23
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.0727853  0.3023505
#sample estimates:
#  mean in group POTUS 
#0.6014493 
#mean in group realDonaldTrump 
#0.4866667 

sentimentTotals1 <- data.frame(colSums(pres[c(1:414),c(33:44)])/414)
sentimentTotals2 <- data.frame(colSums(pres[c(415:864),c(33:44)])/450)
names(sentimentTotals1) <- "count"
names(sentimentTotals2) <- "count"
sentimentTotals1 <- cbind("sentiment" = rownames(sentimentTotals1), sentimentTotals1)
rownames(sentimentTotals1) <- NULL
sentimentTotals2 <- cbind("sentiment" = rownames(sentimentTotals2), sentimentTotals2)
rownames(sentimentTotals2) <- NULL
sentimentTotals1$user <- c("@POTUS")
sentimentTotals2$user <- c("@realDonaldTrump")
sentimentTotals <- rbind(sentimentTotals1,sentimentTotals2)
emotions<-rbind(sentimentTotals1[2:9,],sentimentTotals2[2:9,])
sentiment<-rbind(sentimentTotals1[10:11,],sentimentTotals2[10:11,])
sentimentValence<-rbind(sentimentTotals1[1,],sentimentTotals2[1,])
sentimentValence$sentiment<-c("positive vs. negative")
sent<-rbind(sentiment,sentimentValence)
windows()
ggplot(emotions, aes(reorder(sentiment,count),count,fill=user)) + geom_col(position="dodge") + 
  theme_classic() + coord_flip() +
  scale_fill_manual(values=c("blue","red")) +
  ggtitle("Emotions in @POTUS and @RealDonaldTrump") +
  xlab("") + ylab("Mean NRC Emotion Lexicon Score per Tweet") +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  theme(text=element_text(family="serif")) +
  theme(legend.position=c(.8,.1)) +
  theme(legend.title = element_blank()) +
  theme(legend.background = element_rect(color="black", size=.5)) +
  guides(fill = guide_legend(reverse=TRUE)) +
  theme(panel.grid.major.x=element_line(colour="lightgrey",linetype=1))

windows()
ggplot(sent, aes(sentiment,count,fill=user)) + geom_col(position="dodge") + 
  theme_classic() + coord_flip() +
  scale_fill_manual(values=c("blue","red")) +
  ggtitle("Sentiment in @POTUS and @RealDonaldTrump") +
  xlab("") + ylab("Mean NRC Sentiment Valence per Tweet") +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  theme(text=element_text(family="serif")) +
  theme(legend.position=c(.8,.1)) +
  theme(legend.title = element_blank()) +
  theme(legend.background = element_rect(color="black", size=.5)) +
  guides(fill = guide_legend(reverse=TRUE)) +
  theme(panel.grid.major.x=element_line(colour="lightgrey",linetype=1))

## tf-idf Analysis Based on @POTUS vs. @realDonaldTrump:
library(tidytext)
library(dplyr)
tweets_words <- pres %>% unnest_tokens(word, text) %>%
  count(from_user, word, sort = TRUE) %>%
  ungroup()

tweets_words <- tweets_words  %>% bind_tf_idf(word,from_user, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

library(ggstance)
library(ggthemes)
tweets_words2 <- tweets_words %>% filter(from_user == "realDonaldTrump")
windows()
ggplot(tweets_words2[1:20,], aes(tf_idf, reorder(word,tf_idf), alpha=tf_idf)) +
  theme_classic() + geom_barh(stat = "identity", show.legend = FALSE, fill="red") + 
  scale_alpha(range=c(.5,1)) +
  ggtitle("Top tf-idf Words for @realDonaldTrump Tweets") +
  ylab("") + xlab("tf-idf") +
  scale_x_continuous(expand=c(0,0)) +
  theme(strip.text=element_text(hjust=0)) +
  theme(text=element_text(family="serif")) +
  theme(plot.title=element_text(size=16, hjust=0, face="bold", color="black")) +
  theme(legend.position = "none") +
  theme(panel.grid.major.x=element_line(colour="lightgrey",linetype=1))

tweets_words3 <- tweets_words %>% filter(from_user == "POTUS")
windows()
ggplot(tweets_words3[1:20,], aes(tf_idf, reorder(word,tf_idf), alpha=tf_idf)) +
  theme_classic() + geom_barh(stat = "identity", show.legend = FALSE, fill="blue") +
  scale_alpha(range=c(.5,1)) +
  ggtitle("Top tf-idf Words for @POTUS Tweets") +
  ylab("") + xlab("tf-idf") +
  scale_x_continuous(expand=c(0,0)) +
  theme(strip.text=element_text(hjust=0)) +
  theme(text=element_text(family="serif")) +
  theme(plot.title=element_text(size=16, hjust=0, face="bold", color="black")) +
  theme(legend.position = "none") +
  theme(panel.grid.major.x=element_line(colour="lightgrey",linetype=1))

## STM analysis of @POTUS and @realDonaldTrump Tweets
library(stm)
textPros<-textProcessor(documents=pres$text,metadata=data.frame(pres$sentiment,pres$from_user))

presSTM<-stm(documents=textPros$documents,vocab=textPros$vocab,K=10,
             prevalence=~textPros$meta$pres.sentiment+textPros$meta$pres.from_user,
             content=~textPros$meta$pres.from_user)

# Top words per topic
windows()
par(bty="n",family="serif")
plot.STM(presSTM,type="labels",width=100,n=10,
         main="",family="serif")
title(main="Top 10 Highest Probability Words per Topic",adj=0)
mtext("Based on Structural Topic Model Estimates",adj=0,font=3)

# Top Topics
windows()
par(bty="n",col="grey40",lwd=15,family="serif",adj=0,lend=3)
plot.STM(presSTM,type="summary",
         main="",xlab="",family="serif")
title(main = "Top STM Identified Topics")
mtext("Expected Topic Proportions",adj=0,font=3,col="black")

# Differences per topic 
windows()
par(family="serif",adj=0)
plot.STM(presSTM,type="perspectives",main="",topics=1,
         family="serif")
title(main="Comparison of Word Usage by Account")

presEst<-estimateEffect(c(1:10)~pres.sentiment+pres.from_user,presSTM,
                        metadata=textPros$meta)

windows()
par(bty="n",col="grey40",lwd=15,family="serif",adj=0,lend=3)
plot.STM(presSTM,type="summary",
         main="",xlab="",family="serif")
title(main = "Top STM Identified Topics")
mtext("Expected Topic Proportions",adj=0,font=3,col="black")
par(lwd=1,lend=1,bty="o",col="black") # reset some of the parameters before moving forward.

#########################################
## Topic 1

windows()
par(bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(presEst,                    #Topic proportions in @realDonaldTrump Tweets
                    covariate="pres.sentiment",
                    model=presSTM,
                    topics=presEst$topics[1],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    moderator="pres.from_user",
                    moderator.value="realDonaldTrump",
                    linecol="red",
                    xlim=c(-3,5),
                    printlegend=F)
plot.estimateEffect(presEst,                    #Topic proportions in @POTUS Tweets
                    covariate="pres.sentiment",
                    model=presSTM,
                    topics=presEst$topics[1],
                    method="continuous",
                    moderator="pres.from_user",
                    moderator.value="POTUS",
                    linecol="blue",
                    printlegend=F,add=T)
abline(v=0,lty=1,col="black",lwd=1)
legend("topleft",legend=c("@realDonaldTrump","@POTUS"),col=c("red","blue"),
       lty=1,bty="n",bg="white")
title(main="Topic 1",adj=0)
mtext("great, thank, america, maga, secur, border, use, wonder, work, tune",adj=0,font=3,col="black")

#########################################
## Topic 2

windows()
par(bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(presEst,                    #Topic proportions in @realDonaldTrump Tweets
                    covariate="pres.sentiment",
                    model=presSTM,
                    topics=presEst$topics[2],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    moderator="pres.from_user",
                    moderator.value="realDonaldTrump",
                    linecol="red",
                    xlim=c(-3,5),
                    printlegend=F)
plot.estimateEffect(presEst,                    #Topic proportions in @POTUS Tweets
                    covariate="pres.sentiment",
                    model=presSTM,
                    topics=presEst$topics[2],
                    method="continuous",
                    moderator="pres.from_user",
                    moderator.value="POTUS",
                    linecol="blue",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=1,col="black")
legend("topright",legend=c("@realDonaldTrump","@POTUS"),col=c("red","blue"),
       lty=1,bty="n",bg="white")
title(main="Topic 2",adj=0)
mtext("american, presid, nation, year, day, obama, administr, attack, worker, manufactur",adj=0,font=3,col="black")

#########################################
## Topic 3

windows()
par(bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(presEst,                    #Topic proportions in @realDonaldTrump Tweets
                    covariate="pres.sentiment",
                    model=presSTM,
                    topics=presEst$topics[3],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    moderator="pres.from_user",
                    moderator.value="realDonaldTrump",
                    linecol="red",
                    xlim=c(-3,5),
                    printlegend=F)
plot.estimateEffect(presEst,                    #Topic proportions in @POTUS Tweets
                    covariate="pres.sentiment",
                    model=presSTM,
                    topics=presEst$topics[3],
                    method="continuous",
                    moderator="pres.from_user",
                    moderator.value="POTUS",
                    linecol="blue",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=1,col="black")
legend("topright",legend=c("@realDonaldTrump","@POTUS"),col=c("red","blue"),
       lty=1,bty="n",bg="white")
title(main="Topic 3",adj=0)
mtext("obamacar, fake, russia, healthcar, plan, get, togeth, repealandreplac, insur, immigr",adj=0,font=3,col="black")

#########################################
## Topic 4

windows()
par(bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(presEst,                    #Topic proportions in @realDonaldTrump Tweets
                    covariate="pres.sentiment",
                    model=presSTM,
                    topics=presEst$topics[4],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    moderator="pres.from_user",
                    moderator.value="realDonaldTrump",
                    linecol="red",
                    xlim=c(-3,5),
                    printlegend=F)
plot.estimateEffect(presEst,                    #Topic proportions in @POTUS Tweets
                    covariate="pres.sentiment",
                    model=presSTM,
                    topics=presEst$topics[4],
                    method="continuous",
                    moderator="pres.from_user",
                    moderator.value="POTUS",
                    linecol="blue",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=1,col="black")
legend("topright",legend=c("@realDonaldTrump","@POTUS"),col=c("red","blue"),
       lty=1,bty="n",bg="white")
title(main="Topic 4",adj=0)
mtext("will, watch, stori, time, foxandfriend, scavino, way, trade, even, happen",adj=0,font=3,col="black")

#########################################
## Topic 5

windows()
par(bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(presEst,                    #Topic proportions in @realDonaldTrump Tweets
                    covariate="pres.sentiment",
                    model=presSTM,
                    topics=presEst$topics[5],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    moderator="pres.from_user",
                    moderator.value="realDonaldTrump",
                    linecol="red",
                    xlim=c(-3,5),
                    printlegend=F)
plot.estimateEffect(presEst,                    #Topic proportions in @POTUS Tweets
                    covariate="pres.sentiment",
                    model=presSTM,
                    topics=presEst$topics[5],
                    method="continuous",
                    moderator="pres.from_user",
                    moderator.value="POTUS",
                    linecol="blue",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=1,col="black")
legend("topleft",legend=c("@realDonaldTrump","@POTUS"),col=c("red","blue"),
       lty=1,bty="n",bg="white")
title(main="Topic 5",adj=0)
mtext("today, meet, congratul, week, whitehous, women, need, listen, americanspirit, beauti",adj=0,font=3,col="black")

#########################################
## Topic 6

windows()
par(bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(presEst,                    #Topic proportions in @realDonaldTrump Tweets
                    covariate="pres.sentiment",
                    model=presSTM,
                    topics=presEst$topics[6],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    moderator="pres.from_user",
                    moderator.value="realDonaldTrump",
                    linecol="red",
                    xlim=c(-3,5),ylim=c(0,.2),
                    printlegend=F)
plot.estimateEffect(presEst,                    #Topic proportions in @POTUS Tweets
                    covariate="pres.sentiment",
                    model=presSTM,
                    topics=presEst$topics[6],
                    method="continuous",
                    moderator="pres.from_user",
                    moderator.value="POTUS",
                    linecol="blue",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=1,col="black")
legend("topleft",legend=c("@realDonaldTrump","@POTUS"),col=c("red","blue"),
       lty=1,bty="n",bg="white")
title(main="Topic 6",adj=0)
mtext("trump, hous, join, make, white, countri, better, money, amaz, member",adj=0,font=3,col="black")

#########################################
## Topic 7

windows()
par(bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(presEst,                    #Topic proportions in @realDonaldTrump Tweets
                    covariate="pres.sentiment",
                    model=presSTM,
                    topics=presEst$topics[7],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    moderator="pres.from_user",
                    moderator.value="realDonaldTrump",
                    linecol="red",
                    xlim=c(-3,5),
                    printlegend=F)
plot.estimateEffect(presEst,                    #Topic proportions in @POTUS Tweets
                    covariate="pres.sentiment",
                    model=presSTM,
                    topics=presEst$topics[7],
                    method="continuous",
                    moderator="pres.from_user",
                    moderator.value="POTUS",
                    linecol="blue",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=1,col="black")
legend("topright",legend=c("@realDonaldTrump","@POTUS"),col=c("red","blue"),
       lty=1,bty="n",bg="white")
title(main="Topic 7",adj=0)
mtext("media, big, new, come, news, republican, democrat, win, elect, disast",adj=0,font=3,col="black")

#########################################
## Topic 8

windows()
par(bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(presEst,                    #Topic proportions in @realDonaldTrump Tweets
                    covariate="pres.sentiment",
                    model=presSTM,
                    topics=presEst$topics[8],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    moderator="pres.from_user",
                    moderator.value="POTUS",
                    linecol="blue",
                    xlim=c(-3,5),
                    ylim=c(0,0.18),
                    printlegend=F)
plot.estimateEffect(presEst,                    #Topic proportions in @POTUS Tweets
                    covariate="pres.sentiment",
                    model=presSTM,
                    topics=presEst$topics[8],
                    method="continuous",
                    moderator="pres.from_user",
                    moderator.value="realDonaldTrump",
                    linecol="red",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=1,col="black")
legend("topright",legend=c("@realDonaldTrump","@POTUS"),col=c("red","blue"),
       lty=1,bty="n",bg="white")
title(main="Topic 8",adj=0)
mtext("whitehous, potus, welcom, dem, address, stop, approv, thing, billion, give",adj=0,font=3,col="black")

#########################################
## Topic 9

windows()
par(bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(presEst,                    #Topic proportions in @realDonaldTrump Tweets
                    covariate="pres.sentiment",
                    model=presSTM,
                    topics=presEst$topics[9],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    moderator="pres.from_user",
                    moderator.value="realDonaldTrump",
                    linecol="red",
                    xlim=c(-3,5),
                    printlegend=F)
plot.estimateEffect(presEst,                    #Topic proportions in @POTUS Tweets
                    covariate="pres.sentiment",
                    model=presSTM,
                    topics=presEst$topics[9],
                    method="continuous",
                    moderator="pres.from_user",
                    moderator.value="POTUS",
                    linecol="blue",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=1,col="black")
legend("topleft",legend=c("@realDonaldTrump","@POTUS"),col=c("red","blue"),
       lty=1,bty="n",bg="white")
title(main="Topic 9",adj=0)
mtext("amp, look, flotus, forward, happi, secretari, tremend, help, along, see",adj=0,font=3,col="black")

#########################################
## Topic 10

windows()
par(bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(presEst,                    #Topic proportions in @realDonaldTrump Tweets
                    covariate="pres.sentiment",
                    model=presSTM,
                    topics=presEst$topics[10],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    moderator="pres.from_user",
                    moderator.value="realDonaldTrump",
                    linecol="red",
                    xlim=c(-3,5),
                    printlegend=F)
plot.estimateEffect(presEst,                    #Topic proportions in @POTUS Tweets
                    covariate="pres.sentiment",
                    model=presSTM,
                    topics=presEst$topics[10],
                    method="continuous",
                    moderator="pres.from_user",
                    moderator.value="POTUS",
                    linecol="blue",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=1,col="black")
legend("topleft",legend=c("@realDonaldTrump","@POTUS"),col=c("red","blue"),
       lty=1,bty="n",bg="white")
title(main="Topic 10",adj=0)
mtext("job, honor, sign, readonaldtrump, order, execut, session, one, annouc, hero",adj=0,font=3,col="black")
