#read the file into a df
assn <- read.table("assignment3data.txt", header = T)

#check the df
assn

#question 1 - correlation between S1 and S2 pretraining
cor(assn$S1.pre, assn$S2.pre)
#0.49

#question 2 - correlation between V1 and V2 pretraining
cor(assn$V1.pre, assn$V2.pre)
#0.90

#question 3
#wrt measurement of two distinct constructs, spatial reasoning
#and verbal reasoning, the pattern of correlations pre-training reveals:
cor(assn$S1.pre, assn$V1.pre)
#0.11 divergent

#from quiz
data <- assn
data$S.pre <- (data$S1.pre + data$S2.pre) / 2 
data$V.pre <- (data$V1.pre + data$V2.pre) / 2
cor(data$S.pre, data$V.pre)

#question 4 correlations from the control group could be used to estimate
#test/retest reliability, if so which test is most reliable
cor(assn$S1.pre, assn$S1.post)
#0.57
cor(assn$S2.pre, assn$S2.post)
#0.60
cor(assn$V1.pre, assn$V1.post)
#0.72
cor(assn$V2.pre, assn$V2.post)
#0.91 (highest)

#question 5 does there appear to be a correlation between spatial reasoning
#before training and the amount of improvement in spatial reasoning?
assn$sp.gain <- (assn$S1.post - assn$S1.pre)

cor(assn$S1.pre, assn$sp.gain)
#-0.09 No

#question 6
#Does there appear to be a correlation between verbal reasoning 
#before training and the amount of improvement in verbal reasoning?
assn$vb.gain <- (assn$V1.post - assn$V1.pre)

cor(assn$V1.pre, assn$vb.gain)
#-0.07 No

#question 7
#which group exhibited more improvement in spatial reasoning?
aer <- subset(assn, assn[,2] == 'aer')
des <- subset(assn, assn[,2] == 'des')

mean(aer$S1.post - aer$S1.pre)
#3.66
mean(des$S1.post - des$S1.pre)
#7.14 higher

#question 8 create a color scatterplot matrix for all 4 measures pre test
#do the scatterplots suggest two reliable and valid constructs
pairs(~assn$S1.pre + assn$S2.pre + assn$V1.pre + assn$V2.pre, cex.labels = 1.2)
#no
library(gclus)
pre.r <- abs(cor(pre = cbind(data[3], data[4], data[7], data[8]))) 
cpairs(pre, order.single(pre.r), panel.colors = dmat.color(pre.r), gap=.5)
#yes

#question 9 create a color scatterplot matrix for all 4 measures post test
#do the scatterplots suggest two reliable and valid constructs
pairs(~assn$S1.post + assn$S2.post + assn$V1.post + assn$V2.post, cex.labels = 1.2)
#no

#yes

#question 10
#what is the major change from pre test to post test visible on the color matrix
#All

#Variance
