# K means example-----------


# Generate some example data for clustering
tmp <- c(rnorm(30,-3), rnorm(30,3))
x <- cbind(x=tmp, y=rev(tmp))
plot(x)

#use the kmeans() functions setting K to 2 and nstart=20
km <- kmeans(x, centers = 2, nstart=20 )
#inspect/print the results
km
#Q. How many points are in each cluster?
#Q. What 'component' of your result object details: cluster size, assignement/membership, centers
km$cluster
km$centers
km$size


#plot x colored by the kmeans cluster assignment and add cluster centers as blue points
plot(x, col=km$cluster)
points(km$centers, col = "blue", pch=15)
#Q. Repeat for k=3, which one has the better total SS?
km3 <- kmeans(x, centers = 3, nstart = 20)
km3
km3$withinss








########Hierarchial clustering example-----------
# First we need to calculate point (dis)similarity
#   as the Euclidean distance between observations
dist_matrix <- dist(x)
dist_matrix
#lets look at the class
class(dist_matrix)
View(as.matrix(dist_matrix))
dim(dist_matrix)
dim(as.matrix(dist_matrix))
# The hclust() function returns a hierarchical
#  clustering model
hc <- hclust(d = dist_matrix)
# the print method is not so useful here
hc
# Create hierarchical cluster model: hc
hc <- hclust(dist(x))
# We can plot the results as a dendrogram
plot(hc)
View(hc)
plot(hc)

#Draws a dendogram
plot(hc)
abline(h = 6, col= "red")
cutree(hc, h= 6)
plot(x, col=cutree(hc, k=4))

##########
# Step 1. Generate some example data for clustering
x <- rbind(
  matrix(rnorm(100, mean=0, sd = 0.3), ncol = 2),   # c1
  matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2), # c2
  matrix(c(rnorm(50, mean = 1, sd = 0.3),           # c3
           rnorm(50, mean = 0, sd = 0.3)), ncol = 2))

colnames(x) <- c("x", "y")
# Step 2. Plot the data without clustering
plot(x)
# Step 3. Generate colors for known clusters
#         (just so we can compare to hclust results)
col <- as.factor( rep(c("c1","c2","c3"), each=50) )
plot(x, col=col)


d <- dist(x)
hc <- hclust(d)
plot(hc)
# to return to 2 clusters I could cut at h=2.5 or set k=2
members2 <- cutree(hc, k=2)
# to return to 3 clusters I could cut at h=2 or set k=3
members3<- cutree(hc, k=3)
#lets look at both
members <- cbind(k2=members2, k3=members3)
plot(x, col=members3, pch=15)





# Q. How does this compare to your known 'col' groups?












###################
## Initialize a blank 100 row by 10 column matrix
mydata <- matrix(nrow=100, ncol=10)
## Lets label the rows gene1, gene2 etc. to gene100
rownames(mydata) <- paste("gene", 1:100, sep="")
## Lets label the first 5 columns wt1, wt2, wt3, wt4 and wt5
##   and the last 5 ko1, ko2 etc. to ko5 (for "knock-out")
colnames(mydata) <- c( paste("wt", 1:5, sep=""),
                       paste("ko", 1:5, sep="") )

## Fill in some fake read counts
for(i in 1:nrow(mydata)) {
  wt.values <- rpois(5, lambda=sample(x=10:1000, size=1))
  ko.values <- rpois(5, lambda=sample(x=10:1000, size=1))
  mydata[i,] <- c(wt.values, ko.values)
}
head(mydata)
##lets do PCA
pca<-prcomp(t(mydata), scale = TRUE)
pca
##see what is returned by the prcomp() function
attributes(pca)

# a basic PC1 vs PC2 2-D plot
plot(pca$x [,1], pca$x[,2])

# Variance captured pr PC
pca.var <- pca$sdev^2
pca.var.per<- round(pca.var/sum(pca.var)*100,1)
pca.var.per

barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")

## A vector of colors for wt and ko samples
colvec <- colnames(mydata)
colvec[grep("wt", colvec)] <- "red"
colvec[grep("ko", colvec)] <- "blue"
plot(pca$x[,1], pca$x[,2], col=colvec, pch=16,
     xlab=paste0("PC1 (", pca.var.per[1], "%)"),
     ylab=paste0("PC2 (", pca.var.per[2], "%)"))


## Click to identify which sample is which
identify(pca$x[,1], pca$x[,2], labels=colnames(mydata))

## lets focus on P1 as it accounts for > 90% of variance
loading_scores <- pca$rotation[,1]

summary(loading_scores)

## we are interested in the magnitures of both plus and minus contributing genes
gene_scores <- abs(loading_scores)

