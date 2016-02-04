library(ggplot2)
library(gridExtra)
library(gWidgets)
library(RMySQL)
library(DBI)
library(dbConnect)
library(igraph)
library(googleVis)
library(cluster)
library(fpc)
library(HSAUR)
library(ggdendro)
library(hexbin)


mydb=dbConnect(MySQL(),user='derek',password='derek1',dbname='7cot-fall15',host='130.108.85.104')

hex_har <- ggplot(harasser,aes(signupDateU,lastLoginU)) + stat_binhex() +scale_fill_gradient(low="firebrick1",high="black") + ggtitle("Sign up Date Vs Last Login in harassers") + geom_smooth(size=1,method="lm",linetype=1,se=FALSE,color="blue")
print(hex_har)

hex_normal <- ggplot(complete_normal,aes(signupDateU,lastLoginU)) + stat_binhex() +scale_fill_gradient(low="firebrick1",high="black") + ggtitle("Sign up Date Vs Last Login in Normal Users") + geom_smooth(size=1,method="lm",linetype=1,se=FALSE,color="blue")
print(hex_normal)

# grid.arrange(sud_hardist,sud_normdist,ncol=2)

sud <- ggplot(data=harasser,aes(harasser$signupDateU)) + geom_histogram(aes(y=..density..),fill="firebrick1",alpha=0.3) + geom_density(col="black") + labs(x="Sign Up Date of Harassers",y="Density") + ggtitle("Harassers")
print(sud)

llu <- ggplot(data=harasser,aes(harasser$lastLoginU)) + geom_histogram(aes(y=..density..),fill="firebrick1",alpha=0.3) + geom_density(col="black") + labs(x="Last Login Time of Harassers",y="Density") + ggtitle("Harassers")
print(llu)

grid.arrange(sud,llu,ncol=2)


normal_sud <- ggplot(data=complete_normal,aes(complete_normal$signupDateU)) + geom_histogram(aes(y=..density..),fill="blueviolet",alpha=0.3) + geom_density(col="red") + labs(x="Sign Up Date of Normal Users",y="Density") + ggtitle("Normal Users")
print(normal_sud)

normal_llu <- ggplot(data=complete_normal,aes(complete_normal$lastLoginU)) + geom_histogram(aes(y=..density..),fill="blueviolet",alpha=0.3) + geom_density(col="red") + labs(x="Last Login Time of Normal Users",y="Density") + ggtitle("Normal Users")
print(normal_llu)

# grid.arrange(normal_sud,normal_llu,ncol=2)

r1 <- harasser[,1:2]
distxy <- dist(r1)
hcl <- hclust(distxy) # hierarchial clustering
par(mar=c(5,4,4,2))
plot(hcl,labels=F)
plot(as.dendrogram(hcl),xlab="",sub="")
abline(h=4e+07,col="blue")
ggdendrogram(hcl,rotate=TRUE,size=4,theme_dendro=FALSE,color="tomato")

r1_mat <- as.matrix(r1)
r1_mat_all <- as.matrix(harasser[,1:8])
r1_mat_all_scaled <- scale(r1_mat_all)
heatmap(r1_mat,col=cm.colors(25))

distxy_2 <- dist(harasser[,1:8])
hc_all <- hclust(distxy_2)
plot(as.dendrogram(hc_all))
plot(hc_all,labels=F)
abline(h=4e+07,col="red")


heatmap(r1_mat_all_scaled,col=cm.colors(10),labels=F) # heatmap 

h <- harasser[,1:8]
km <- kmeans(h,centers=3)
plotcluster(h,km$cluster,pch=20,cex=0.8,main="K-Means Clustering") # K means Clustering

s <- svd(scale(r1_mat_all)) # Singular Value Decomposition
par(mfrow=c(1,2))
plot(s$v[,2],xlab="Column",ylab="Second Right Singular Vector",pch=20,cex=0.7)
plot(s$d,xlab="Column",ylab="Singular Values",main="Singular Values",pch=20,cex=0.7)
plot(s$d^2/sum(s$d^2),xlab="Column",ylab="Proportion of Variance Explained",pch=20,cex=0.7)


