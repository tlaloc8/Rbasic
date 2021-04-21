library(cluster)
library(psych)
library(ggfortify)
library(ggplot2)


data(mtcars)
summary(mtcars)
str(mtcars)

#ScatterPlott
plot(disp~hp,mtcars)
# z nazwami 
with(mtcars,text(disp~hp, labels=rownames(mtcars), pos=3,cex=.5))


library(reshape) #NORMALIZowaC
x <- rescaler(mtcars)

#Matryca dystancji dist 
matryca_dystancji<-dist(x, method = "euclidean") 
matryca_dystancji
print(matryca_dystancji, digits = 3)


#niehier K means
niehierarchiczny<-kmeans(matryca_dystancji,5, iter.max =30)
niehierarchiczny

mtcars[niehierarchiczny$cluster==1, 1:2]

palette(c("goldenrod2","seagreen3","red","royalblue4","indianred3"))

#Grafiki z nazwami cars
plot(cmdscale(matryca_dystancji), xlab="First Dimension", ylab="Second Dimension", main="K-means clustering, k=5", cex.sub=0.7, font.lab=2, pch="*")
text(cmdscale(matryca_dystancji),labels=rownames(mtcars),cex=0.7, font=2, font.axis=2, pos=4,col=niehierarchiczny$cluster)

#hier dendogram with complete linekage 
hc<-hclust(matryca_dystancji)
plot(hc)


