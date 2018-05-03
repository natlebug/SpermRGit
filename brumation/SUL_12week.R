#=====SUL ANALYSIS======#
sul1<-cbind(X12_week$sul)
histsul<-log(X12_week$sul)
summary(histsul)
hist(histsul) #Logging the 'weight' results normalized the data
kruskal.test(sul ~ treat, data=X12_week)
kruskal.test(sul ~ week, data=X12_week)
kruskal.test(sul ~ anno, data=X12_week)

#===sul Anova and interaction analysis====#
sulaov<-aov(sul ~ week + anno + treat + week*treat + anno*treat + anno*week, data=X12_week)
aov(sulaov)
summary(sulaov)
sulaus<-glm(sul ~ week + anno + treat + week*anno + week*treat + treat*anno, data=X12_week)
glm(sulaus)
summary(sulaus)
sul1<-as.factor(X12_week$sul)
levels(sul1) #This shows you the numbers that you have in each line
sulaus1<-cbind(X12_week, sul=as.factor(X12_week$sul))
head(X12_week)#cbind for the data defines each of the numbers and categories that you are trying to analyse and allows you to "slice" the data later as you define what you where you need to analyse the interactions.
sulaus1fact<-as.factor(X12_week$sul)
levels(sul1)
trtbywk<-glm(sul ~ week*treat, data=X12_week)
summary(trtbywk)
X12_week<-cbind(X12_week, time=as.factor(X12_week$week))
head(X12_week)
X12_week<-cbind(X12_week, brum=as.factor(X12_week$treat))
head(X12_week)
wtaus2<-glm(sul ~ time*brum, data=X12_week)
summary(wtaus2)

#=====SMI ANALYSIS======#
SMI1<-cbind(X12_week$SMI)
histSMI<-log(X12_week$SMI)
summary(histSMI)
hist(histSMI) #Logging the 'weight' results normalized the data
kruskal.test(SMI ~ treat, data=X12_week)
kruskal.test(SMI ~ week, data=X12_week)
kruskal.test(SMI ~ anno, data=X12_week)

#===sul Anova and interaction analysis====#
SMIaov<-aov(smi ~ week + anno + treat, data=X12_week)
aov(SMIaov)
summary(SMIaov)
SMIaov1<-aov(smi ~ treat, data=X12_week)
aov(SMIaov)
summary(SMIaov1)
SMIaus<-glm(smi ~ week + anno + treat + week*treat + anno*treat +anno*week, data=X12_week)
glm(SMIaus)
summary(SMIaus)
SMI1<-as.factor(X12_week$sul)
levels(SMI1) #This shows you the numbers that you have in each line
SMIaus1<-cbind(X12_week, SMI=as.factor(X12_week$sul))
head(X12_week)#cbind for the data defines each of the numbers and categories that you are trying to analyse and allows you to "slice" the data later as you define what you where you need to analyse the interactions.
SMIaus1fact<-as.factor(X12_week$sul)
levels(SMIaus1fact)
trtbywk<-glm(smi ~ week*treat, data=X12_week)
summary(trtbywk)
X12_week<-cbind(X12_week, time=as.factor(X12_week$week))
head(time)
X12_week<-cbind(X12_week, brum=as.factor(X12_week$treat))
head(brum)
SMIaus2<-glm(smi ~ week*treat, data=X12_week)
summary(SMIaus2)
