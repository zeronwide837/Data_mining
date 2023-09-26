setwd("폴더 주소")

##data overview
diabetes = read.table("diabetes.txt")
head(diabetes)
dim(diabetes)
str(diabetes)

##data processing
#removing meaningless data
diabetes = diabetes[2:6]
diabetes2 = scale(diabetes)
diabetes2 = as.data.frame(diabetes2)

#add variable names
colnames(diabetes2) <- c("RW","FPG","UPGC","UPIC","SSPG")
head(diabetes2)
str(diabetes2)

#creating new variables
n = nrow(diabetes2)
diabetes2$x1 = rnorm(n,0,1)
diabetes2$x2 = factor(sample(c("A","B","C","D"), size=n, replace=TRUE))
head(diabetes2)

#creating dummy variables
library(dummies)

x2.dum = dummy(diabetes2$x2)
head(x2.dum)

diabetes3 = cbind(diabetes2,x2.dum)
head(diabetes3)

#removing useless variable
diabetes3 = diabetes3[,-7]
head(diabetes3)

attach(diabetes3)

##visualization
library(rgl)

plot3d(FPG,UPGC,UPIC)
#FPG와 UPGC간에 강한 선형관계가 포착됨
plot3d(UPGC,UPIC,SSPG)
plot3d(RW,UPGC,UPIC)
plot3d(RW,UPGC,SSPG)

##Hierarchical clustering
h.clust1 = hclust(dist(diabetes2[1:50,]), method = "complete")
plot(h.clust1)

h.clust2 = hclust(dist(diabetes2[1:50,]), method = "average")
plot(h.clust2)

h.clust3 = hclust(dist(diabetes2[1:50,]), method = "single")
plot(h.clust3)

##cut the dendrograms
h.cut1 = cutree(h.clust1, k=4)
h.cut1

h.cut1h = cutree(h.clust1, h=3)
h.cut1h

h.cut2 = cutree(h.clust2, k=4)
h.cut2

h.cut2h = cutree(h.clust2, h=2.3)
h.cut2h

##Showing the results

h.seg1 = diabetes3[h.cut1 == 1,]
h.seg2 = diabetes3[h.cut1 == 2,]
h.seg3 = diabetes3[h.cut1 == 3,]
h.seg4 = diabetes3[h.cut1 == 4,]

pie(table(h.cut1),main="number of observations in segment")
h.mean = rbind(apply(h.seg1,2,mean),apply(h.seg2,2,mean),apply(h.seg3,2,mean),apply(h.seg4,2,mean))
rownames(h.mean) = c(1,2,3,4)
h.mean
dist(h.mean, method="euclidean", diag=T)

#3D plot
plot3d(RW,FPG,SSPG,col=h.cut1)

#Boxplot
par(mfrow=c(2,2))
boxplot(h.seg1[,1],h.seg2[,1],h.seg3[,1],h.seg4[,1],ylab=names(h.seg1)[1],xlab="segment",col="blue",names=c(1,2,3,4))
boxplot(h.seg1[,2],h.seg2[,2],h.seg3[,2],h.seg4[,2],ylab=names(h.seg1)[2],xlab="segment",col="blue",names=c(1,2,3,4))
boxplot(h.seg1[,3],h.seg2[,3],h.seg3[,3],h.seg4[,3],ylab=names(h.seg1)[3],xlab="segment",col="blue",names=c(1,2,3,4))
boxplot(h.seg1[,4],h.seg2[,4],h.seg3[,4],h.seg4[,4],ylab=names(h.seg1)[4],xlab="segment",col="blue",names=c(1,2,3,4))
par(mfrow=c(1,1))


## K-means clustering
k.clust = kmeans(diabetes3[,1:5],centers=3,nstart=1)
k.clust$cluster
k.clust$tot.withinss

k.clust2 = kmeans(diabetes3[,1:5],centers=3)
k.clust2$cluster
k.clust2$tot.withinss

##Showing the results
pie(k.clust$size, main = "number of observations in segment")
k.clust$centers
dist(k.clust$centers, method="euclidean", diag = T)

#3D Plot
plot3d(RW,FPG,SSPG,col=k.clust$cluster)

#Boxplot
seg1 = diabetes2[k.clust$cluster==1,]
seg2 = diabetes2[k.clust$cluster==2,]
seg3 = diabetes2[k.clust$cluster==3,]

par(mfrow=c(2,2))
boxplot(seg1[,1],seg2[,1],seg3[,1],ylab=names(seg1)[1],xlab="segment",col="blue",names=c(1,2,3))
boxplot(seg1[,2],seg2[,2],seg3[,2],ylab=names(seg1)[2],xlab="segment",col="blue",names=c(1,2,3))
boxplot(seg1[,3],seg2[,3],seg3[,3],ylab=names(seg1)[3],xlab="segment",col="blue",names=c(1,2,3))
boxplot(seg1[,4],seg2[,4],seg3[,4],ylab=names(seg1)[4],xlab="segment",col="blue",names=c(1,2,3))
par(mfrow=c(1,1))
