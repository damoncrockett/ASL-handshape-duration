# PSYC 201b Group Project
# Damon Crockett, Qi Cheng, Anne Therese Frederiksen

######################
## Import and Clean ##
######################

d = read.csv('handshape_and_frequency_clean_var2.csv')
d$X = NULL

d$ohs = as.logical(d$ohs)
d$ohd = as.logical(d$ohd)
d$thss = as.logical(d$thss)
d$thsd = as.logical(d$thsd)
d$thds = as.logical(d$thds)
d$thdd = as.logical(d$thdd)

# creates single factor from 6 Booleans
d$type = names(d[7:12])[apply(d[7:12] == 1, 1, which)]
d$type = as.factor(d$type)

d$ohs = NULL
d$ohd = NULL
d$thss = NULL
d$thsd = NULL
d$thds = NULL
d$thdd = NULL

write.csv(d,'/home/damoncrockett/PSYC201/group_project/handshape_and_frequency_cleaner_var2.csv')

#####################
####   Analysis  ####
#####################

dc = read.csv('handshape_cleaner_var_compound.csv')
d = read.csv('handshape_and_frequency_cleaner_var2.csv')

# models

library(lme4)

m0 = lmer(data=d, duration ~ 1 +
            (1|consultant))

m1 = lmer(data=d, duration ~ 1 +
            (1|consultant) +
            (1|gloss))

m2 = lmer(data=d, duration ~ type +
               (1|consultant) +
               (1|gloss))

m3 = lmer(data=d, duration ~ Lg10WF +
               (1|consultant) +
               (1|gloss))

m4 = lmer(data=d, duration ~ Lg10WF + type +
               (1|consultant) +
               (1|gloss))

m5 = lm(data=d, Lg10WF ~ type)


summary(m4)
summary(m2)
anova(m5)

# ANOVAs

anova(m0,m1,m2)
anova(m1,m3,m4)

# the compound glosses

qplot(dc$type)
qplot(d$type)


#####################
#####   Plots  ######
#####################

library(ggplot2)

ggplot(d, aes(x=Lg10WF)) + 
  geom_histogram(fill='white', color='black', binwidth = .1) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"),
        plot.background = element_rect(fill = "white"),
        axis.text = element_text(size=rel(.9), color='black'),
        text = element_text(family='arial', face='plain', color='black', size = 18)) +
  labs(x='LOG FREQUENCY',y='COUNT', title='COUNTS BY LOG FREQUENCY')

ggplot(d, aes(x=duration)) + 
  geom_histogram(fill='white', color='black', binwidth = 2) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"),
        plot.background = element_rect(fill = "white"),
        axis.text = element_text(size=rel(0.9), color='black'),
        text = element_text(family='arial', face='plain', color='black', size = 18)) +
  labs(x='DURATION IN TENS OF MILLISECONDS',y='COUNT', title='COUNTS BY DURATION')

d$ordered_type = d$type
d$ordered_type = factor(d$ordered_type, levels(d$ordered_type)[c(1,3,5,2,4,6)]) 

ggplot(d, aes(ordered_type, color=type)) + 
  geom_histogram(aes(fill=type)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"),
        plot.background = element_rect(fill = "white"),
        axis.text = element_text(size=rel(1.2), color='black'),
        text = element_text(family='arial', face='plain', color='black', size = 18)) +
  labs(x='SIGN TYPES ORDERED BY ASCENDING DURATION',y='COUNT', title='COUNTS BY SIGN TYPE')

# note: this package must be installed separately

library(coefplot2)

coefplot2(m4,
          cex.var = 1,
          cex.pts = 2,
          cex.axis = 5)

coefplot2(m2,
          cex.var = 2,
          cex.pts = 2,
          cex.axis = 5)

coefplot2(m5,
          cex.var = 1,
          cex.pts = 2,
          cex.axis = 5)


w = read.csv('word_length.csv')

ggplot(w[w$length<20,], aes(x=Lg10WF, y = length)) + 
  geom_point(color='black', alpha=0.1) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"),
        plot.background = element_rect(fill = "white"),
        axis.text = element_text(size=rel(0.9), color='black'),
        text = element_text(family='arial', face='plain', color='black', size = 22)) +
  labs(x='LOG FREQUENCY IN SUBTLEX CORPUS',y='WORD LENGTH IN ENGLISH', title='WORD LENGTH BY SUBTLEX LOG FREQUENCY') +
  stat_smooth(method='lm', formula=y~x, se=T)

with(w,(cor.test(Lg10WF,length)))

qplot(d$Lg10WF)
qplot(log10(d$FREQcount + 1))
qplot(d$FREQcount)
qplot(d$SUBTLWF)


