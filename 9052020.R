#Zadanie

#Package readxl can read archives from excel .xls and .xlsx
library(readxl)
base<-read.csv("USArrests1.csv")
base$NEW<-1
row.names(base)<-base$Province 

base2<-base[,-grep(c("Murder"), colnames(base))]


matrizcor<-cor(USArrests)
matrizcov<-cov(USArrests)

eigen(matrizcor)

comp<-princomp(USArrests,cor=T)
summary(comp) 
comp$loadings 
comp$sdev^2 #wariancje
screeplot(comp) 
biplot(comp)
