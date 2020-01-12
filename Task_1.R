#Task 1
library(readr)
library(class)
library(cluster)
library(MASS)





#Read in teeth data
teeth <- read_csv("OneDrive - University of Reading/PHD DATA MINING/Coursework/R Code/Coursework/DataMiningPhDCoursework/teeth.csv")

#Check for missing data
sum(is.na(teeth))
labels = c(teeth[,1])
#Scale/Normalise data - pre-processing
teeth_scale <- teeth
teeth_scale$TopInc <- scale(teeth_scale$TopInc)
teeth_scale$BotInc <- scale(teeth_scale$BotInc)
teeth_scale$TopCan <- scale(teeth_scale$TopCan)
teeth_scale$BotCan <- scale(teeth_scale$BotCan)
teeth_scale$TopPre <- scale(teeth_scale$TopPre)
teeth_scale$BotPre <- scale(teeth_scale$BotPre)
teeth_scale$TopMol <- scale(teeth_scale$TopMol)
teeth_scale$BotMol <- scale(teeth_scale$BotMol)
teeth_scale

#PCA - pre-processing
teeth_pca <- prcomp(teeth_scale[, 2:9], scale = TRUE)
biplot(teeth_pca)

#Hierarchical Clustering
dist_matrix <- dist(teeth_scale[, 2:9], method = 'euclidean')
hclusters <- hclust(dist_matrix, method = 'complete')
plot(hclusters)
rect.hclust(hclusters , k = 15, border = 2:6)
abline(h = 1.5, col = 'red')

#Cluster Validity - silhouette method
num_clusters <- pam(dist_matrix, k=15)
sil <- silhouette(num_clusters)
plot(sil)


axis(side=2,at=ticks,labels=labels)
sil_av <- c()

for(i in 2:30){
  num_clusters <- pam(dist_matrix, k=i)

  sil_av <- append(sil_av,num_clusters$silinfo$avg.width)
  
  }
as.data.frame(sil_av)

