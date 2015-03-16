# PSYC 201b Group Project
# Damon Crockett

######################
## Import and Clean ##
######################

d = read.csv('handshape_clean.csv')
d$X = NULL

d$ohs = as.logical(d$ohs)
d$ohd = as.logical(d$ohd)
d$thss = as.logical(d$thss)
d$thsd = as.logical(d$thsd)
d$thds = as.logical(d$thds)
d$thdd = as.logical(d$thdd)

d$type = names(d[4:9])[apply(d[4:9] == 1, 1, which)]
d$type = as.factor(d$type)

d$ohs = NULL
d$ohd = NULL
d$thss = NULL
d$thsd = NULL
d$thds = NULL
d$thdd = NULL

write.csv(d,'/home/damoncrockett/PSYC201/group_project/handshape_cleaner.csv')

#####################
####   Analysis  ####
#####################

d0 = read.csv('handshape_cleaner.csv')
d = read.csv('handshape_and_frequency_cleaner.csv')


library(lme4)

summary(lmer(data=d, duration ~ type +
               (1|consultant)))

summary(lmer(data=d0, duration ~ type +
               (1|consultant)))


d0$oh = ((d0$type == 'ohs') | (d0$type == 'ohd'))
d0$diff = d0$type == 'ohd' | d0$type == 'thdd' | d0$type == 'thsd' | d0$type == 'thds'
d0$th_diff = d0$type == 'thdd' | d0$type == 'thsd' | d0$type == 'thds'
d0$nd_diff = d0$type == 'thdd' | d0$type == 'thsd'
d0$nd_diff = d0$type == 'thdd' | d0$type == 'thsd'
d0$thsd = d0$type == 'thsd'

summary(lmer(data=d0, duration ~ oh +
               (1|consultant)))

summary(lmer(data=d0, duration ~ diff +
               (1|consultant)))

summary(lmer(data=d0, duration ~ th_diff +
               (1|consultant)))

summary(lmer(data=d0, duration ~ nd_diff +
               (1|consultant)))

summary(lmer(data=d0, duration ~ thsd +
               (1|consultant)))


#####################
#####   Plotz  ######
#####################

library(ggplot2)
ggplot(d0[d0$duration < 150,],aes(consultant,duration)) + geom_point(aes(color=consultant))
ggplot(d0[d0$duration < 150,],aes(type,duration)) + geom_point(aes(color=consultant))

ggplot(d0[d0$duration < 150,],aes(type)) + geom_histogram()
ggplot(d0[d0$duration < 150,],aes(type,duration)) + geom_boxplot(aes(color=type))

ggplot(d,aes(freq,duration)) + geom_point(aes(color=consultant))
ggplot(d,aes(consultant,duration)) + geom_point(aes(color=consultant))
ggplot(d,aes(type,duration)) + geom_point(aes(color=consultant))
ggplot(d,aes(type,freq)) + geom_point(aes(color=consultant))






#####################
#####  Tablez  ######
#####################

table(d0$type)

library(plyr)

by_type = ddply(d0, c('type'), summarize,
                duration=mean(duration))














