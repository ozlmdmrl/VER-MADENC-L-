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
veriler$ýlkbagistansonragecenay<- as.numeric(veriler$ýlkbagistansonragecenay)

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

hist(veriler$sonbagistansonrakiaylar, col="red", main = "Son baðýþtan sonraki aylarýn histogram Grafiði")
hist(veriler$toplambagis, col="red", main = "Toplam baðýþ histogram Grafiði")
hist(veriler$toplamkan, col="red", main = "Toplam kan histogram Grafiði")
hist(veriler$ýlkbagistansonragecensure, col="red", main = "Ýlk baðýþtan sonra geçen sürenin histogram Grafiði")

frekansbagislama <- table(veriler$bagislama)
barplot(frekansbagislama, col="purple" , main="Kan Baðýþlama 
Grafiði",xlab="Kiþi Sayýsý",ylab = "Baðýþlama Durumu" , horiz = TRUE) 

boxplot(veriler$sonbagistansonrakiaylar, col="blue", main="Son Baðýþtan Sonraki Aylarýn Kutu Grafiði")
boxplot(veriler$toplambagis, col="blue", main="Toplam Baðýþ Kutu Grafiði")
boxplot(veriler$toplamkan, col="blue", main="Toplam Kan Kutu Grafiði")
boxplot(veriler$ýlkbagistansonragecenay, col="blue", main="Ýlk Baðýþtan sonra Geçen Süre Kutu Grafiði")

pairs( ~ sonbagistansonrakiaylar +  toplambagis + toplamkan + ýlkbagistansonragecenay , data= veriler, 
       col=" dark green", main= "Serpilme Diyagramlarý")

View(modelC11)

summary(veriler)
