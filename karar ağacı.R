veriler= read.table(file.choose(), header = T, sep = ";")

library(RWeka)
library(rJava)
library(partykit)
library(plyr)
library(caret)

data(veriler)
View(veriler)
str(veriler)
summary(veriler)
attributes(veriler)

data(veriler)
veriler$bagislama<- as.character(veriler$bagislama)
veriler$sonbagistansonrakiaylar <- as.numeric(veriler$sonbagistansonrakiaylar)
veriler$toplambagis<- as.numeric(veriler$toplambagis)
veriler$toplamkan <- as.numeric(veriler$toplamkan)
veriler$�lkbagistansonragecenay<- as.numeric(veriler$�lkbagistansonragecenay)

veriler$bagislama <- revalue(veriler$bagislama, c("1"="var","0"="yok"))

#Rweka peketi icinde C4.5  algoritmasinin J48() isimli bir uyarlamasi yer almaktadir.

head(veriler)
veriler$bagislama<- as.factor(veriler$bagislama)
model<- J48(bagislama~.,data = veriler)

#kurallari gorelim
print(model)

summary(model)

#grafigini cizelim
plot(model)

hist(veriler$sonbagistansonrakiaylar, col="red", main = "Son ba���tan sonraki aylar�n histogram Grafi�i")
hist(veriler$toplambagis, col="red", main = "Toplam ba��� histogram Grafi�i")
hist(veriler$toplamkan, col="red", main = "Toplam kan histogram Grafi�i")
hist(veriler$�lkbagistansonragecensure, col="red", main = "�lk ba���tan sonra ge�en s�renin histogram Grafi�i")

frekansbagislama <- table(veriler$bagislama)
barplot(frekansbagislama, col="purple" , main="Kan Ba���lama 
Grafi�i",xlab="Ki�i Say�s�",ylab = "Ba���lama Durumu" , horiz = TRUE) 

boxplot(veriler$sonbagistansonrakiaylar, col="blue", main="Son Ba���tan Sonraki Aylar�n Kutu Grafi�i")
boxplot(veriler$toplambagis, col="blue", main="Toplam Ba��� Kutu Grafi�i")
boxplot(veriler$toplamkan, col="blue", main="Toplam Kan Kutu Grafi�i")
boxplot(veriler$�lkbagistansonragecenay, col="blue", main="�lk Ba���tan sonra Ge�en S�re Kutu Grafi�i")

pairs( ~ sonbagistansonrakiaylar +  toplambagis + toplamkan + �lkbagistansonragecenay , data= veriler, 
       col=" dark green", main= "Serpilme Diyagramlar�")

View(modelC11)

summary(veriler)
