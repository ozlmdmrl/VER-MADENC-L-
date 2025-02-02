veriler= read.table(file.choose(), header = T, sep = ";")

library(caret)
library(cluster)
library(plyr)

View(veriler)
summary(veriler)
str(veriler)
attributes(veriler)

veriler$bagislama <- as.factor(veriler$bagislama)
veriler$sonbagistansonrakiaylar <- as.numeric(veriler$sonbagistansonrakiaylar)
veriler$toplambagis<- as.numeric(veriler$toplamkan)
veriler$�lkbagistansonragecenay <- as.numeric(veriler$�lkbagistansonragecenay)
veriler$toplamkan<- as.numeric(veriler$toplamkan)

veriler$bagislama <- revalue(veriler$bagislama, c("1"="var","0"="yok"))

#sadece num degerlerden olusan alt kume
n_veriler <- veriler [,c(1,2,3,4)]

set.seed(1234)

ind <- sample(1:748,20)
veriler <- n_veriler[ind,]

model <- agnes(veriler,metric = "manhattan",method = "single") #manhattan uzakl��a g�re
modelo <- agnes (veriler, metric = "euclidien", method="single") #oklit uzakliga gore

#gorsellestirelim
pltree(model, main="en yakin komsu algoritmasi ile kumeleme")
pltree(modelo, main="en yakin komsu algoritmasi ile kumeleme")

#s�n�f etiketi �eklinde g�rmek istersek
veriler$bagislama <- revalue(veriler$bagislama, c("1"="var","0"="yok"))

pltree(model, main="en yak�n kom�u algoritmas� ile k�meleme", labels=veriler$bagislama)
bannerplot(agnes(veriler),main="Bannerplot Grafigi", labels = veriler$bagislama)

