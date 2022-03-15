veriler = read.table(file.choose(),header = T,sep = ";")

library(caret)
install.packages("e1071")
library(e1071)
library(plyr)

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

set.seed(1)
verisetibolme <- createDataPartition(y=veriler$bagislama, p=0.6, list=FALSE)
#veri setini egitim ve test olarak rastgele ikiye ayiracagiz
egitim <- veriler[verisetibolme,]
test <- veriler[-verisetibolme,]

testNitelikleri <- test[,-5]
testHedefNitelik <- test[[5]]
egitimNitelikleri <- egitim [,-5]
egitimHedefNitelik <- egitim [[5]]


naiveBayes_modeli_kuruldu <- naiveBayes(egitimNitelikleri, egitimHedefNitelik)
naiveBayes_modeli_kuruldu

(tahminiSiniflar <- predict(naiveBayes_modeli_kuruldu, testNitelikleri))

(karisiklikmatrisi <- table(tahminiSiniflar, testHedefNitelik, dnn =c ("Tahmini 
Siniflar", "Gercek Siniflar")))

(TP <- karisiklikmatrisi [1])
(FP <- karisiklikmatrisi [3])
(FN <- karisiklikmatrisi [2])
(TN <- karisiklikmatrisi [4])


paste0("Dogruluk = ",(Dogruluk <- (TP+TN)/sum(karisiklikmatrisi)))
paste0("Hata = ",(Hata <- 1-Dogruluk))














