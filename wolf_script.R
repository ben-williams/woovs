#/\/\/\/\\///\/\\\/\/\/\/\/\/\/\
#Examination of McRoberts and Mech 2014. Wolf Population Regulation Revisited - Again. The Journal of Wildlife Management. 78(6): 963-967
# author ben william - ben.williams@alaska.gov
# 2015_07_24
############################################
#Note: I've added in a "location" and "ID" variable
#import data structured as a .csv 
dat <- read.csv("dat.csv")
#divide deer BMI by 1000 to match results in the paper
dat$deer <- dat$deer/1000
#add inverse BMI column for weights
dat$iBMI <- 1/dat$deer
head(dat)

# create pruned dataset
prune <- subset(dat,removal!='30')

#data fields are:
   #population - the location of the population
   #deer - Ungulate biomass index/1000 km^2 <- (bcw: is this really biomass or numbers?)
   #wolves - wolves/1000 km2
   #removal - populations with "normal" relationships = in,
      #others are marked as recolonized, coyote hybrids, or harvest >30
   #id - the popoulations setup for repeated measures (random effects) evaluation

#basic examination all data
library(ggplot2)
#set plotting parameters
theme_set(theme_bw(base_size=12)+
             theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank()))
          

# data examinations
# plot all data (black) and removals data (red)
ggplot(dat, aes(deer, wolves))+geom_point(color='red', size=3)+geom_point(data=prune, aes(deer, wolves), size=3)

# by location
ggplot(dat, aes(deer, wolves, color=locs))+geom_point(size=3)

ggplot(dat, aes(deer, wolves, color=locs, group=locs, fill=locs))+geom_point(size =3)+
   stat_smooth(method='gam', formula=y~s(x, k=3), alpha=.3)

#############################################################
# Models - I used nls for analysis in order to be able to compare nested models with AIC
# type 1 model
m1 <- nls(wolves~b1*deer, data=dat, start=list(b1=.2))
dat$m1 <- fitted(m1, type='response')

# type 2 model
m2 <- nls(wolves~(b1*deer)/(b2+deer), data=dat, start=list(b1=.2, b2=.2))
dat$m2 <- fitted(m2, type='response')

# type 3 model
m3 <- nls(wolves~(b1*deer^2)/(b2+deer^2), data=dat, start=list(b1=.2, b2=.2))
dat$m3 <- fitted(m3, type='response')

# quad model - excluded for now
#m4 <- nls(wolves~b1*deer+b2*deer^2, data=dat, start=list(b1=.2, b2=.2))
#dat$m4 <- fitted(m4, type='response')

# plot model results
library(reshape2)
mdat <- melt(dat, id.vars = c('population', 'deer', 'wolves', 'removal','id', 'locs', 'iBMI'), value.name='est')
colnames(mdat)[8] <- 'Model'
ggplot(mdat, aes(deer, est, color=Model))+geom_line()+scale_x_continuous(breaks=0:13)
############################################

#############################################################
# Same thing on pruned data
m1p <- nls(wolves~b1*deer, data=prune, start=list(b1=.2))
prune$m1 <- fitted(m1p, type='response')

# type 2 model
m2p <- nls(wolves~(b1*deer)/(b2+deer), data=prune, start=list(b1=.2, b2=.2))
prune$m2 <- fitted(m2p, type='response')

# type 3 model
m3p <- nls(wolves~(b1*deer^2)/(b2+deer^2), data=prune, start=list(b1=.2, b2=.2))
prune$m3 <- fitted(m3p, type='response')

# quad model - excluded for now
#m4p <- nls(wolves~b1*deer+b2*deer^2, data=prune, start=list(b1=.2, b2=.2))
#prune$m4 <- fitted(m4p, type='response')

# plot model results
mprune <- melt(prune, id.vars = c('population', 'deer', 'wolves', 'removal','id', 'locs', 'iBMI'), value.name='est')
colnames(mprune)[8] <- 'Model'
ggplot(mprune, aes(deer, est, color=Model))+geom_line()+scale_x_continuous(breaks=0:13)

# Difference between all data (solid lines) and pruned data (dashed lines) 
ggplot(mprune, aes(deer, est, color=Model))+geom_line(lty=4)+geom_line(data=mdat, aes(deer, est, color=Model))


# model comparisons
AIC(m1,m2,m3)
logLik(m1); logLik(m2); logLik(m3)

library(MuMIn)
AICc(m1, m2, m3)

# same for pruned data
AIC(m1p,m2p,m3p)
logLik(m1p); logLik(m2p); logLik(m3p)

library(MuMIn)
AICc(m1p, m2p, m3p)


# the weighted models take a bit more to come up with equivalent AIC values etc.

#How I would model this
dat1 <- dat
dat1$dum <- 1
m7 <- gam(wolves~s(deer, k=4)+s(locs, bs='re', by=dum)+id, data=dat1, gamma=1.4)
AIC(m7)
logLik(m7)
newd <- expand.grid(deer=seq(1,12,.5), id=levels(dat1$id), dum=0, locs=c('E','M','N'))
newd$pred <- predict(m7, newd, type='response')
ggplot(newd, aes(deer,pred))+stat_summary(fun.data='mean_cl_boot', geom='smooth', B=1000,
                                          fill='#999999',alpha=.1,color='#999999', fill='gray')
#taking into account the differences in "location" and the "sample sites" get a a way different relationship. Likely because what is being argued back and forth isn't meaningful based upon the data used in the analysis.
