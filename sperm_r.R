setwd("Desktop/Rstudio")
sperm=read.table('Sper_rep.csv', header=T)
View(sperm_rfile)
View(Sper_rep)

shapiro.test(sperm_rfile$totcon)

sperm1<-cbind(sperm_rfile, log(sperm_rfile$totcon))
sperm1<-cbind(Sper_rep, log(Sper_rep$t2sp))

hist(sperm1$"log(sperm_rfile$totcon)")

Hist(sperm1$"log10(sperm_rfile$totcon)")

kruskal.test(totcon ~ trtnum, data=sperm_rfile)
kruskal.test(totcon ~ mon, data=sperm_rfile)
kruskal.test(totcon ~ male, data=sperm_rfile)

kruskal.test(resp ~ trtnum, data=sperm_rfile)
kruskal.test(resp ~ mon, data=sperm_rfile)
kruskal.test(resp ~ male, data=sperm_rfile)

kruskal.test(timepeak ~ trtnum, data=sperm_rfile)
kruskal.test(timepeak ~ mon, data=sperm_rfile)
kruskal.test(timepeak ~ male, data=sperm_rfile)

kruskal.test(peakcon ~ trtnum, data=sperm_rfile)
kruskal.test(peakcon ~ mon, data=sperm_rfile)
kruskal.test(peakcon ~ male, data=sperm_rfile)

kruskal.test(avconml ~ trtnum, data=sperm_rfile)
kruskal.test(avconml ~ mon, data=sperm_rfile)
kruskal.test(avconml ~ male, data=sperm_rfile)

kruskal.test(avconsample ~ trtnum, data=sperm_rfile)
kruskal.test(avconsample ~ mon, data=sperm_rfile)
kruskal.test(avconsample ~ male, data=sperm_rfile)

kruskal.test(permot ~ trtnum, data=sperm_rfile)
kruskal.test(permot ~ mon, data=sperm_rfile)
kruskal.test(permot ~ male, data=sperm_rfile)

kruskal.test(pernonmot ~ trtnum, data=sperm_rfile)
kruskal.test(pernonmot ~ mon, data=sperm_rfile)
kruskal.test(pernonmot ~ male, data=sperm_rfile)

kruskal.test(sop1 ~ trtnum, data=sperm_rfile)
kruskal.test(sop1 ~ mon, data=sperm_rfile)
kruskal.test(sop1 ~ male, data=sperm_rfile)

kruskal.test(sop2 ~ trtnum, data=sperm_rfile)
kruskal.test(sop2 ~ mon, data=sperm_rfile)
kruskal.test(sop2 ~ male, data=sperm_rfile)

#anova for the kruskal-wallis results that appear significant. 
#Also, is there a linear relationship between month and the other 
#significant factors.

aus<- glm(sop1 ~ trtnum + mon + trtnum*mon, data=sperm_rfile)
glm(aus)
summary(aus)

aus1<- glm(avconml ~ trtnum + mon + trtnum*mon, data=sperm_rfile)
glm(aus1)
summary(aus1)

aus2<- glm(permot ~ trtnum + mon + trtnum*mon, data=sperm_rfile)
glm(aus2)
summary(aus2)

aus3<-glm(pernonmot ~ trtnum + mon + trtnum*mon, data=sperm_rfile)
glm(aus3)
summary(aus3)

aus4<-glm(resp ~ trtnum + mon + trtnum*mon, data=sperm_rfile)
glm(aus4)
summary(aus4)

aus5<-glm(timepeak ~ trtnum + mon + trtnum*mon, data=sperm_rfile)
glm(aus5)
summary(aus5)


# compare models
fit1 <- glm(permot ~ trtnum + mon + trtnum*mon, data=sperm_rfile)
glm(fit1) 



library(boot)

library(car)
plot(permot ~ trtnum, data=sperm_rfile, main="(a)")
abline(lm(permot~trtnum, data=sperm_rfile)) #abile makes the line on the graph that represents
       
plot(sop1 ~ trtnum, data=sperm_rfile, main="(a)")
abline(lm(sop1~trtnum, data=sperm_rfile)) #abile makes the line on the graph that represents

plot(sop2 ~ trtnum, data=sperm_rfile, main="(a)")
abline(lm(sop2~trtnum, data=sperm_rfile)) 

plot(avconml ~ trtnum, data=sperm_rfile, main="(a)")
abline(lm(avconml~trtnum, data=sperm_rfile)) 

plot(timepeak ~ trtnum, data=sperm_rfile, main="(a)")
abline(lm(timepeak~trtnum, data=sperm_rfile)) 

plot(peakcon ~ trtnum, data=sperm_rfile, main="(a)")
abline(lm(peakcon~trtnum, data=sperm_rfile)) 

plot(peakcon ~ mon, data=sperm_rfile, main="(a)")
abline(glm(peakcon~mon, data=sperm_rfile)) 

#=================================================#
sperm1<-glm(peakcon ~ trtnum + month + trtnum*month, data=sperm_rfile)
summary(sperm1)
sperm2<-glm(avconml ~ trtnum + mon + trtnum*mon, data=sperm_rfile)
summary(sperm2)
sperm3<-glm(sop1 ~ trtnum + mon + trtnum*mon, data=sperm_rfile)
summary(sperm3)
sperm4<-glm(sop2 ~ trtnum + mon + trtnum*mon, data=sperm_rfile)
summary(sperm4)
sperm5<-glm(resp ~ trtnum + month + month*trtnum, data=sperm_rfile)
summary(sperm5)
sperm6<-glm(timepeak ~ trtnum + mon + trtnum*mon, data=sperm_rfile)
summary(sperm6)
sperm7<-glm(permot ~ trtnum + mon + trtnum*mon, data=sperm_rfile)
summary(sperm7)
sperm8<-glm(pernonmot ~ trtnum + mon + trtnum*mon, data=sperm_rfile)
summary(sperm8)


#======#Slicing the data when you have interactions#=====#
#PEAK CONCENTRATION INTERACTION SLICE#
(sperm_rfile$trtnum)
trtnumfact<-as.factor(sperm_rfile$trtnum)
levels(trtnumfact)
sperm_rfile<-cbind(sperm_rfile, trtnumfact=as.factor(sperm_rfile$trtnum))
head(sperm_rfile)

(sperm_rfile$month)
monfact<-as.factor(sperm_rfile$month)
levels(monfact)
sperm_rfile<-cbind(sperm_rfile, monfact=as.factor(sperm_rfile$month))
head(sperm_rfile)

#=====concentration=====#
#PEAK CONCENTRATION INTERACTION SLICE#
sperm1<-glm(peakcon ~ trtnumfact + monfact + trtnumfact*monfact, data=sperm_rfile)
summary(sperm1)

#=======motility========#
#PEAK CONCENTRATION INTERACTION SLICE#
sperm2<-glm(permot ~ trtnumfact + monfact + trtnumfact*monfact, data=sperm_rfile)
summary(sperm2)

sperm3<-glm(pernonmot ~ trtnumfact + monfact + trtnumfact*monfact, data=sperm_rfile)
summary(sperm3)

#=========response======#
#RESPONSE CONCENTRATION INTERACTION SLICE#
sperm4<-glm(resp ~ trtnumfact + monfact + trtnumfact*monfact, data=sperm_rfile)
summary(sperm4)

#=======================#
View(Sper_rep)
shapiro.test(Sper_rep$t2sp)

sperm1<-cbind(Sper_rep, log(Sper_rep$t2sp))
hist(sperm1$t2sp)
hist(sperm1$"log(Sper_rep$t2sp)")

(Sper_rep$t2sp)
sperm2<-as.factor(Sper_rep$t2sp)
levels(sperm2)
Sper_rep<-cbind(Sper_rep, sperm2=as.factor(Sper_rep$t2sp))
head(Sper_rep)
#The above script binds the titles to the data so the computer knows what its analysing. "as.factor" does this.
(Sper_rep$spend)
sperm3<-as.factor(Sper_rep$spend)
level(sperm3)
Sper_rep<-cbind(Sper_rep, sperm3=as.factor(Sper_rep$spend))
head(Sper_rep)

sperm1<-glm(t2sp ~ hor + dat +anno + hor*dat, data=Sper_rep)
summary (sperm1)

sperm2<-glm(spend ~ hor + dat +anno + hor*dat, data=Sper_rep)
summary(sperm2)
#
(Sper_rep$spdur)
sperm4<-as.factor(Sper_rep$spdur)
levels(sperm4)
Sper_rep<-cbind(Sper_rep, sperm4=as.factor(Sper_rep$spdur))
head(Sper_rep)

sperm4<-glm(spdur ~ hor + dat + hor*dat, data=Sper_rep)
summary(sperm4)
#
(Sper_rep$rep24)
sperm5<-as.factor(Sper_rep$rep24)
levels(sperm5)
Sper_rep<-cbind(Sper_rep, sperm5=as.factor(Sper_rep$rep24))
head(Sper_rep)

sperm5<-glm(rep24 ~ hor + dat + anno + hor*dat, data=Sper_rep)
summary(sperm5)
#
(Sper_rep$rep48)
sperm6<-as.factor(Sper_rep$rep48)
levels(sperm6)
Sper_rep<-cbind(Sper_rep, sperm6=as.factor(Sper_rep$rep48))
head(Sper_rep)

sperm6<-lm(rep48 ~ hor + dat + anno + hor*dat, data=Sper_rep)
summary(sperm6)
#
(Sper_rep$rep72)
sperm7<-as.factor(Sper_rep$rep72)
levels(sperm7)
Sper_rep<-cbind(Sper_rep, sperm6=as.factor(Sper_rep$rep72))
head(Sper_rep)

sperm6<-lm(rep72 ~ hor + dat + anno + hor*dat, data=Sper_rep)
summary(sperm6)
