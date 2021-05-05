#Analiza dyskryminacyjna 
library(MASS)
mydata<-mtcars
mydata
fit <- lda(as.factor(cyl)~mpg+disp+hp+wt+drat, data=mydata,na.action="na.omit", CV=TRUE)
fit2 <- lda(as.factor(cyl)~mpg+disp+hp+wt+drat, data=mydata,na.action="na.omit" )
fit
fit$class
fit$posterior

fit

ct <- table(mtcars$cyl, fit$class)
ct
#proportion 
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))

#datos centrados por coeficientes de las funciones discriminantes
scores<-scale(as.matrix(mydata[,c("mpg","disp","hp","wt","drat")]),scale=FALSE)%*%(fit2$scaling[,1:2])
 #centroides centrados por coeficientes de las funciones discriminantes
centroscores<-scale(as.matrix(fit2$means[,]),scale=FALSE)%*%(fit2$scaling[,1:2])
 ##grafica funciones discriminantes
plot(scores[,1],scores[,2],type="n",main="evaluaciones de las fun. discr.",xlab="fun.discr.1",ylab="fun.discr.2")
text(scores[,1],scores[,2],cex=.5,labels=fit$class,col=c(fit$class))
points(centroscores[1:3,1], centroscores[1:3,2], pch=13,col=c(1,2,3),cex=1.5)  
    
library(klaR)
partimat(as.factor(cyl)~mpg+disp+hp+wt+drat ,data=mydata,method="lda")
partimat(as.factor(cyl)~mpg+disp+hp+wt+drat ,data=mydata,method="qda")
# Scatterplot for 3 Group Problem
pairs(mydata[c("mpg","disp","hp","wt","drat")], main="mtcars ", pch=22,col= (mydata$cyl))
 #misma grafica otros colores 
pairs(mydata[c("mpg","disp","hp","wt","drat")], main="mtcars ", pch=22,
           +  bg=c("red", "yellow", "blue")[unclass(as.factor(mydata$cyl))])
#misma grafica otra presentacion 
pairs(mydata[c("mpg","disp","hp","wt","drat")], main="mtcars ", pch=unclass(as.factor(mydata$cyl)),col=unclass(as.factor(mydata$cyl))) 


#Fisher 
library(lfda)
iris
k <- iris[, -5]
y <- iris[, 5]
r <- 3
z<-lfda(k, y, r, metric = "plain")
library(klaR)
partimat(cyl~mpg+disp+hp+wt+drat,data=mtcars,method="lda")
