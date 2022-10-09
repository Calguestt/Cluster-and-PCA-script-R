
#read the file 
amino<-read.csv(file.choose(), header = T)
#check its the right one 
amino
aminoScaled<-scale(amino)
aminoScaled
attach(amino)
names(amino)
#create new object for the distance metric
dist.amino<-dist(amino, method = "binary",diag = F, upper = F)
#use hclust function against the distance metric you just created, (method="average" is the same as UPGMA)
hiclamino<-hclust(dist.amino, method = "average")
#plot the clustergram
plot(hiclamino, hang = -1)
rect.hclust(hicamino, k = 11, border = "red")
plot(hiclamino1, xlab = "", hang = -1, cex=0.8, main = "", ylab = "")

dist.damino
#change the clustergram into a dendrogram
aminodend<-as.dendrogram(hiclamino)
#plot the dendrogram 

plot(aminodend, label = Protein, horiz = TRUE, main="",  xlab = "", ylab = "", cex=0.5)
rect.dendrogram(aminodend, k=4, horiz = T)

plot(aminodend, horiz = TRUE, main="UPGMA", xlab = "", ylab = "Protein", cex=0.7)
rect.dendrogram(aminodend, k=4)


aminoclusters<-cutree(hiclamino,5)
aminoclusters
plot(amino,col =tclusters)



plot(aminodend, main="UPGMA", xlab = "", ylab = "Species", cex=0.7)
rect.dendrogram(aminodend, k=4)

#perform PCA 
aminopca<-prcomp(amino[, -1])
str(aminopca)
aminopca$x
amino2<-cbind(amino, aminopca$x[,1:20])
amino2
plot(aminopca, type ="l", main = "")
names(pca)
summary(amino2)
#plot the biplot 
biplot(pca)
#adjust the biplot as you want (cex=..... this is the font size)
biplot(aminopca, scale = 1, cex=1,)
summary(aminopca)
library("ggbiplot")
install_github("vqv/ggbiplot")

install.packages("devtools")
library("devtools")

ggbiplot(aminopca)
bplot3= ggbiplot(pcobj = aminopca, 
                 choices = c(2, 1), 
                 obs.scale = 0, var.scale = 0,
                 labels = row.names(amino), 
                 varname.size = 6, 
                 varname.abbrev = T, 
                 var.axes = T, 
                 circle = F, 
                 ellipse = T, groups = amino$"Character")
print(bplot3)

install.packages("ape")
library("ape")
plot(as.phylo(hiclcomp), type = "fan", cex = 0.8)
