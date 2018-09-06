setwd("Desktop/Rstudio")
View(Sper_rep)
shapiro.test(Sper_rep$t2sp)

sperm1<-cbind(Sper_rep, log(Sper_rep$t2sp))
hist(sperm1$t2sp)
hist(sperm1$"log(Sper_rep$t2sp)")

(Sper_rep$t2sp)
sperm2<-as.factor(Sper_rep$t2sp)
levels(sperm2)
Resp<-cbind(Sper_rep, sperm2=as.factor(Sper_rep$t2sp))
head(Sper_rep) #The above script binds the titles to the data so the computer knows what its analysing. "as.factor" does this.
#

(Sper_rep$spend)
sperm3<-as.factor(Sper_rep$spend)
level(sperm3)
Sper_rep<-cbind(Sper_rep, sperm3=as.factor(Sper_rep$spend))
head(Sper_rep)



sperm1<-glm(t2sp ~ hor + dat + anno, data=Sper_rep) 
summary(sperm1)
sperm1a<-glm(Resp ~ hor*dat, data=Sper_rep)
summary (sperm1a)

sperm2<-glm(spend ~ hor + dat + anno, data=Sper_rep)
summary(sperm2)
sperm2a<-glm(spend ~ hor*dat, data=Sper_rep)
summary(sperm2a)

(Sper_rep$spdur)
sperm4<-as.factor(Sper_rep$spdur)
levels(sperm4)
Sper_rep<-cbind(Sper_rep, sperm4=as.factor(Sper_rep$spdur))
head(Sper_rep)

sperm4<-glm(spdur ~ hor + dat + anno, data=Sper_rep)
summary(sperm4)
sperm4a<-glm(spdur ~ hor*dat, data=Sper_rep)
summary(sperm4a)


#
(Sper_rep$rep24)
sperm5<-as.factor(Sper_rep$rep24)
levels(sperm5)
Sper_rep<-cbind(Sper_rep, sperm5=as.factor(Sper_rep$rep24))
head(Sper_rep)

sperm5<-glm(rep24 ~ hor + dat + anno, data=Sper_rep) 
summary(sperm5)
sperm5a<-glm(rep24 ~ hor*dat, data=Sper_rep)
summary(sperm5a)
#
(Sper_rep$rep48)
sperm6<-as.factor(Sper_rep$rep48)
levels(sperm6)
Sper_rep<-cbind(Sper_rep, sperm6=as.factor(Sper_rep$rep48))
head(Sper_rep)

sperm6<-lm(rep48 ~ hor + dat + anno + hor*dat, data=Sper_rep)
summary(sperm6)
sperm6a<-lm(rep48 ~ hor*dat, data=Sper_rep)
summary(sperm6a)
#
(Sper_rep$rep72)
sperm7<-as.factor(Sper_rep$rep72)
levels(sperm7)
Sper_rep<-cbind(Sper_rep, sperm6=as.factor(Sper_rep$rep72))
head(Sper_rep)

sperm7<-lm(rep72 ~ hor + dat + anno, data=Sper_rep)
summary(sperm7)
sperm7a<-lm(rep72 ~ hor*dat, data=Sper_rep)
summary(sperm7a)
