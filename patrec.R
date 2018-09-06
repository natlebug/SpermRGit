View(patrec)
shapiro.test(patrec$egg)
shapiro.test(patrec$emb)

sperm<-cbind(patrec, log(patrec$egg))
hist(sperm$"log(patrec$egg)")
sperm<-cbind(patrec, log(patrec$emb))
hist(sperm$"log(patrec$emb)")

kruskal.test(egg ~ anno, data=patrec)
kruskal.test(egg ~ year, data=patrec)

kruskal.test(emb ~ anno, data=patrec)
kruskal.test(emb ~ year, data=patrec)


egg<- glm(egg ~ anno, data=patrec)
glm(egg)
summary(egg)

emb<- glm(emb ~ anno, data=patrec)
glm(emb)
summary(emb)

egg<- glm(egg ~ year, data=patrec)
glm(egg)
summary(egg)

emb<- glm(emb ~ year, data=patrec)
glm(egg)
summary(egg)
