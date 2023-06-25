
df <- read.csv(file='insurance.csv', stringsAsFactors = FALSE)
summary(df)
df_clus = df

set.seed(123)

## Clustering
head(df_clus)

#Data transformation and normalizing 
df_clus$sex = ifelse(df_clus$sex == 'female',1,0)
df_clus$smoker = ifelse(df_clus$smoker == 'yes',1,0)

df_clus$region = as.factor(df_clus$region)
df_clus$region = as.numeric(df_clus$region)
head(df_clus)
df.norm <- data.frame(sapply(df_clus, scale))

head(df.norm)

#K means clustering
library(factoextra)
fviz_nbclust(df.norm, kmeans, nstart = 10, k.max = 20, method = "silhouette")

cluster <- kmeans(df.norm, centers = 2, nstart = 10)

cluster$centers
cluster$size
cluster$betweenss/cluster$tot.withinss



#PCA + Clustering
pca.out = prcomp(df.norm[,1:6]) # perform PCA on all columns
summary(pca.out)

pca.out$rotation

scores = as.data.frame(pca.out$x)

pca.var <- pca.out$sdev^2
pca.pve <- data.frame( pve = pca.var/sum(pca.var), component = c(1:6))  
# plot
g <- ggplot(pca.pve, aes(component, pve))  
g + geom_point() + labs(title="Scree Plot", x="Component Number", y="PVE") +
  scale_x_continuous(breaks=seq(1,6,by=1))

fviz_nbclust(scores[,1:5], kmeans, nstart = 10, k.max = 20, method = "silhouette")
clus.out <- kmeans(scores[,1:5], centers = 18, nstart = 10)
# ratio of between-cluster variation to within-cluster variation
clus.out$size
clus.out$betweenss/clus.out$tot.withinss

# visualize the cluster using PC1 & PC2
scores$cluster <- as.character(clus.out$cluster)
g <- ggplot(scores, aes(PC1, PC2)) 
g + geom_text(aes(label=cluster, color=cluster),hjust=0, vjust=0)

# plot the weight of the original variables in each PC
rot <- as.data.frame(pca.out$rotation)  
rot$feature <- rownames(rot)

# ordered in decreasing loading (i.e., weight) for PC1
rot$feature <- factor(rot$feature, levels= rot$feature[order(rot$PC1, decreasing=T)])
ggplot(rot, aes(feature, PC1)) + 
  geom_bar(stat="identity", position="identity") +   
  theme(axis.text.x=element_text(size = 15, angle=45, hjust=1)) 

# ordered in decreasing loading for PC2
rot$feature <- factor(rot$feature, levels= rot$feature[order(rot$PC2, decreasing=T)])
ggplot(rot, aes(feature, PC2)) + 
  geom_bar(stat="identity", position="identity") +   
  theme(axis.text.x=element_text(size=15, angle=45, hjust=1)) 

#hierarchical clustering

dist <- dist(df.norm, method = "euclidean")
hc <- hclust(dist, method = "single")
plot(hc, hang = -1, ann = FALSE)
member <- cutree(hc, k = 2)
table(member)
plot(hc, hang = -1, ann = FALSE) 
rect.hclust(hc, k =2, border = "red")

# Interpreting CLusters

df.norm$cluster_label_1 = clus.out$cluster 
df.norm$cluster_label_1 = as.factor(df.norm$cluster_label_1)
df.norm$cluster_label_2 = member
df.norm$cluster_label_2 = as.factor(df.norm$cluster_label_2)

aggdf1 <- aggregate(cbind(age,sex,bmi,children,smoker,region,charges) ~ cluster_label_1, data=df.norm, mean )
aggdf1
aggdf2 <- aggregate(cbind(age,sex,bmi,children,smoker,region,charges) ~ cluster_label_2, data=df.norm, mean )
aggdf2


# Visualize Clusters

df_clus$cluster_label_1 = df.norm$cluster_label_1

#visualize
library(ggplot2)
g <- ggplot(df_clus,aes(age,charges)) 
g + geom_text(aes(label=cluster_label_1, color=cluster_label_1),hjust=0, vjust=0)

g <- ggplot(df_clus,aes(age,charges)) 
g + geom_text(aes(label=cluster_label_1, color=bmi),hjust=0, vjust=0)

g <- ggplot(df_clus,aes(age,charges)) 
g + geom_text(aes(label=cluster_label_1, color=smoker),hjust=0, vjust=0)

g <- ggplot(df_clus,aes(smoker,charges)) 
g + geom_text(aes(label=cluster_label_1, color=cluster_label_1),hjust=0, vjust=0)

g <- ggplot(df_clus,aes(children,charges)) 
g + geom_text(aes(label=cluster_label_1, color=cluster_label_1),hjust=0, vjust=0)

g <- ggplot(df_clus,aes(region,charges)) 
g + geom_text(aes(label=cluster_label_1, color=cluster_label_1),hjust=0, vjust=0)

g <- ggplot(df_clus,aes(charges,region)) 
g + geom_text(aes(label=cluster_label_1, color=region),hjust=0, vjust=0)

g <- ggplot(df_clus,aes(sex,charges)) 
g + geom_text(aes(label=cluster_label_1, color=cluster_label_1),hjust=0, vjust=0)

g <- ggplot(df_clus,aes(bmi,charges)) 
g + geom_text(aes(label=cluster_label_1, color=cluster_label_1),hjust=0, vjust=0)

g <- ggplot(df_clus,aes(bmi,charges)) 
g + geom_text(aes(label=cluster_label_1, color=smoker),hjust=0, vjust=0)
