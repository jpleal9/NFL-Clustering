#Import libraries ----
library(corrplot)
library(psych)
library(GPArotation)
library(gpairs)
library(dplyr)
library(grt)
library(cluster)
library(mclust) 
library(tidyverse)
library(factoextra)
library(xlsx)
#DataSet Import ----
df = read.delim(file.choose(), header=TRUE, sep = ";", dec = ".")

#Variables summary ----
summary (df)

#Input variables crop ----
df_inputs <- df[,6:28]

round(var(df_inputs),2)

#Total.Salary and Signing.Bonus transformation to numeric values
df$Total.Salary = gsub(".00","",df$Total.Salary)
df$Signing.Bonus = gsub(".00","",df$Signing.Bonus)

df$Total.Salary = gsub(",","",df$Total.Salary)
df$Signing.Bonus = gsub(",","",df$Signing.Bonus)

df$Total.Salary = str_remove(df$Total.Salary, "[$]")
df$Signing.Bonus = str_remove(df$Signing.Bonus, "[$]")

df$Total.Salary = as.integer(df$Total.Salary)
df$Signing.Bonus = as.integer(df$Signing.Bonus)

df$Signing.Bonus[is.na(df$Signing.Bonus)] = 0


#Scatterplot ----
#Lots of data, better to see in zoom
pairs(df_inputs, lower.panel = NULL, pch = 20)

plot(df_inputs$Speed,df_inputs$Acceleration, xlab = "Speed", ylab = "Acceleration", main = "Speed vs Acceleration")
#Plot that represents Direct Proporcion between two variables

plot(df_inputs$Release,df_inputs$Finesse.Moves, xlab = "Release", ylab = "Finesse Moves", main = "Release vs Finesse Moves")
#Plot that represents Inverse Proporcion between two variables

plot(df$Ball.Carrier.Vision,df$Play.Recognition, xlab = "Carrier Vision", ylab = "Play Recognition", main = "Carrier Vision vs Play Recognition")
#Plot representing two independent variables


#From the scatterplot representation it is possivel to observe several positive correlations (speed and acceleration for example)
#and some negative ones (finesse moves and release) and even some unrelated ones (carrier vision and play recognicion).
#Howeve, after a general analysis on the data it is possible to say that the correlation is very positive

#Correlation ----
correlation = cor(df_inputs)
round(correlation,2)

#Corrplot ----
par(oma = c(2, 2, 2, 2)) # space around for text 
corrplot.mixed(correlation,
order = "hclust", #order of variables 
tl.pos = "lt", #text left + top
upper = "ellipse",number.cex=0.5)

#In the Corrplot it is possible to see that the data is highly correlated, but now in a more
#visual and simple representation (lot's of thin blue ellipses, that represent a good correlation)

#PCA ----

#Bartett and KMO ----
cortest.bartlett(correlation)

KMO(correlation)
#Awareness e Toughness have very low MSA levels

#Component extration ----

#Data scale ----
datascale = scale(df_inputs)

#Principal Components Extraction
pc23 <- principal(datascale, nfactors=23, rotate="none", scores=TRUE)

#Principal components variance
round(pc23$values,3)

#Screeplot - Find the elbow
plot(pc23$values, type = "b", main = "Scree plot", xlab = "Number of PC", ylab = "Eigenvalue")

#According to the graphic and the Kaiser criterium it is best to reduce the sample
#to 3 PC's, or even 4 PC's

#Maximum Principal component analysis ----
pc23$loadings

#1st PC: "Light" players, with good offensive features (high and positive speed, acceleration, agility, carrying, etc.)
#and low defensive features (negative strenght, tackle and hit power)
#2nd PC: "Light" players, with good defensive features (high and positive pursuit, play recognition, tackle, hit power
#and finess moves) and low offensive features (negative break tackle, throw power and carrying)
#3rd PC: "Heavier" players, for example, tight ends or defensive ends (high and positive awareness, strenght, trucking and toughness;
#negative speed, acceleration, agility and change of direction)
#4rd PC: High awareness and stamina, a bit more complicated to check a pattern. Perhaps offensive linemen?
#5th PC and up: Very complex to be analysed 
pc23$communality

#3 Components Method ----
#Unrotated
pc3 = principal(datascale, nfactors=3, rotate="none")
pc3$loadings

#3 Components Varimax
pc3v = principal(datascale, nfactors=3, rotate="varimax") 
pc3v$loadings

#3 Components Oblimin (Since variables are highly related)
pc3o = principal(datascale, nfactors=3, rotate="oblimin") 
pc3o$loadings

#Communalities of both rotated methods
round(pc3v$communality,3)
round(pc3o$communality,3)

#Not a big difference between both rotations, so we will opt for the Varimax, since it is
#the simplest to explain

#4 Components Method ----
#Unrotated
pc4 = principal(datascale, nfactors=4, rotate="none")
pc4$loadings

#4 Components Varimax
pc4v = principal(datascale, nfactors=4, rotate="varimax") 
pc4v$loadings

#4 Components Oblimin (Since variables are highly related)
pc4o = principal(datascale, nfactors=4, rotate="oblimin") 
pc4o$loadings

#Communalities of both rotated methods
round(pc4v$communality,3)
round(pc4o$communality,3)

#If we opt for the 4 components method then the Oblimin rotation is the best option,
#since it has a better loadings distribution 

#Number of PC's choice----
pc3v$loadings
pc4o$loadings

round(pc3v$communality,3)
round(pc4o$communality,3)


#The 4 PC's method can represent 5% more than the 3 PC's method, but it's a little more complicated
#to interpret (it can incorporate special teams players, like kickers or punters, or even quarterbacks).
#The trade-off is not positive.
#About the communalities the 4 PC's method represents some variables like awareness and stamina better
#than the 3 components method. However it's still complicated to define the 4th component.
#Given that we will opt for the 3 components method with Varimax rotation

#PC's Interpretation
pc3v$loadings
#1st PC: Has previously mentioned this component represents athletic offensive players
#2nd PC: This one represents athletic defensive players
#3rd PC: This component represents strong players but not too athletic. It can be associated with players that are in the
#first lines, both offensive and defensive, where the principal quality is strenght

#PC Scores----
pc3sc = principal(datascale,nfactors = 3,rotate = "none",scores = TRUE)

round(pc3sc$scores,3)

#PC1
mean(pc3sc$scores[,1])
sd(pc3sc$scores[,1])

#PC2
mean(pc3sc$scores[,2])
sd(pc3sc$scores[,2])

#PC3
mean(pc3sc$scores[,3])
sd(pc3sc$scores[,3])

#Saving the new variables on the dataset----
df$AtlOff = pc3sc$scores[,1]
df$AtlDef = pc3sc$scores[,2]
df$ToughOvrl = pc3sc$scores[,3]

#PC1 vs PC2 Scatterplot
plot(df$AtlOff, df$AtlDef, pch = 20, xlab="PC1", ylab="PC2", main = "Scores: PC1 vs Pc2")
#text(df[1:500,]$AtlOff, df[1:500,]$AtlDef-0.1, df[1:500,2], cex = 0.5) #(x,y,labels)


#Clustering----
#Exporting to a new dataset just the name of the players and the PC values
df_pca = dplyr::select(df,Full.Name,AtlOff,AtlDef,ToughOvrl)
df_pca_vls = df_pca[,2:4]
df_pca_name = df_pca[,1]

#Generalized Pairs Plot
gpairs(df_pca[2:4])

#From this plots it's possible to see some separation between some components. 
#Between the two first PC's (Athletic Offensive and Athletic Defensive) it can be seen 3 big groups, with some values between the 3 groups
#Between Athletic Defensive and Tough Overall it can be seen a separation between 2 big groups and more intermediate point than the first plot
#Between Athletic Ofensive and Tough Overall it's a bit more difficult to see a pattern. However there is a group (with negative values) that looks
#a bit isolated from the big cluster. It can be seen about 3 smaller groups inside the big group

#Hierarchical Clustering----
#Euclidian Complete (with standardization)
#Top vars to define the smaller plots

df_dist = dist(df_pca_vls)
df_dist_top = dist(df_pca_vls[1:50,])

h_clust_df_ec = hclust(df_dist)
h_clust_df_ec_top = hclust(df_dist_top)

plot(h_clust_df_ec,label = df_pca_name,cex = 0.5, hang = -1)
plot(h_clust_df_ec_top,label = df_pca_name[1:50],cex = 0.7, hang = -1)


#Euclidian Simple (with standardization)
#Top vars to define the smaller plots

h_clust_df_es = hclust(df_dist,method='single')
h_clust_df_es_top = hclust(df_dist_top,method='single')

plot(h_clust_df_es,label=df_pca_name,hang=-1, cex = 0.3)
plot(h_clust_df_es_top,label=df_pca_name[1:50],hang=-1, cex = 0.7)

#Euclidian with Ward algorithm
#Top vars to define the smaller plots

h_clust_df_ew = hclust(df_dist,method='ward.D2') 
h_clust_df_ew_top = hclust(df_dist_top,method='ward.D2') 


plot(h_clust_df_ew,label=df_pca_name,hang=-1, cex = 0.3)
plot(h_clust_df_ew_top,label=df_pca_name[1:50],hang=-1, cex = 0.7)

#Cutting the dendrogram

# 6 clusters 
plot(h_clust_df_ew, labels = FALSE, hang = -1)
groups.k6 = cutree(h_clust_df_ew, k=6) 
rect.hclust(h_clust_df_ew, k=6, border="blue") 
aggregate(df_pca_vls,list(groups.k6), mean)

# 4 clusters
groups.k4 = cutree(h_clust_df_ew, k=4) 
rect.hclust(h_clust_df_ew, k=4, border="red") 
aggregate(df_pca_vls,list(groups.k4), mean)


#Silhouette validation

#6 clusters
plot(silhouette(groups.k6,df_dist),col=1:6, border=NA)

#4 clusters
plot(silhouette(groups.k4,df_dist),col=1:4, border=NA)

#Week structure. Better with k=6 (0.41)

#K-means clustering----

#Defining the number of clusters using screeplot
wssplot = function(xx, nc=15, seed=1000){
  wss = (nrow(df_pca_vls)-1)*sum(apply(df_pca_vls,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] = sum(kmeans(xx, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares", main = "Screeplot")}

wssplot(df_pca_vls, nc=7)

#Analysing the elbow (around 3 clusters) the number of clusters applied will be one above that: 4 clusters

#Clusters definition

kmeans.k4 <- kmeans(df_pca_vls, 4,nstart=100) 
kmeans.k4$centers
kmeans.k4$cluster
kmeans.k4$size 
plot(silhouette(kmeans.k4$cluster,df_dist),col = 1:4,border = NA)

#It's is possible to see an improvement on the data, with the average silhouette width
#at 0.42 (0.1 more comparing to the Euclidean with Ward with k = 6)
#Various nstart values were experimented, but the results didn't vary very much

#K-means Classification

#AtlOff vs AtlDef
k_means_o = order(kmeans.k4$cluster)
data.frame(df_pca$Full.Name[k_means_o],kmeans.k4$cluster[k_means_o])
plot(df_pca$AtlOff, df_pca$AtlDef, type="p", xlab="Athletic Offensive", ylab="Athletic Defensive", col=kmeans.k4$cluster+1) 

#Well defined clusters. The first analysis made was simmilar to what this model gave, only with one more cluster
#because of the k-value used

#AtlOff vs ToughOvrl
plot(df_pca$AtlOff, df_pca$ToughOvrl, type="p", xlab="Athletic Offensive", ylab="Tough Overall", col=kmeans.k4$cluster+1) 
#The group that was tought to be separate from the others was included in one of the clusters

#AtlDef vs ToughOvrl
plot(df_pca$AtlDef, df_pca$ToughOvrl, type="p", xlab="Athletic Defensive", ylab="Tough Overall", col=kmeans.k4$cluster+1) 
#In this representation there is some mixture in the data. The 2 data hipotesis would be better for this comparisson

#PAM Clustering

pam.k4 <- pam(df_pca_vls,4)

#AtlOff vs AtlDef
table(groups.k4,pam.k4$clustering)
plot(df_pca$AtlOff, df_pca$AtlDef, type="p", xlab="Athletic Offensive", ylab="Athletic Defensive",col=pam.k4$clustering+1)
#Very similar to the former, which confirms that the model is working correctly and there are no "weird" values, like
#outliers or nulls

#AtlOff vs ToughOvrl
plot(df_pca$AtlOff, df_pca$ToughOvrl, type="p", xlab="Athletic Offensive", ylab="Tough Overall",col=pam.k4$clustering+1)

#AtlDef vs ToughOvrl
plot(df_pca$AtlDef, df_pca$ToughOvrl, type="p", xlab="Athletic Defensive", ylab="Tough Overall",col=pam.k4$clustering+1)

#In all plots the simmilarities are evident, so it can be concluded that the data is good


#All plots
clPairs(df_pca_vls, pam.k4$clustering)



#Model Based Clustering----

#GMM (Gaussian Mixture Model)
mclust_result = Mclust(df_pca_vls)
summary(mclust_result)

#According to this function the best model to cluster the data is divide the data in 9 clusters, with the VVV model
#(elipsoidal clusters, with variable volumes, shapes and orientations)


#BIC values used to choose the number of clusters
fviz_mclust(mclust_result, "BIC", palette = "jco")

#The plot represents a better fit for the model with 9 clusters

# Plot results
plot(mclust_result, what = "density")

plot(mclust_result, what = "classification") 

plot(mclust_result, what = "uncertainty")

# Adding the columns to the inicial dataset ----

df$Classification_GMM = mclust_result$classification
# Probabilistic model

df$Classification_kmeans = kmeans.k4$cluster
#K-Means model

df$Classification_hierarquic = groups.k6
#Hierarchical model

#Clusters profiling ----

write.xlsx(df, file = "madden_profiling.xlsx")
#To export the dataset to be analysed in Excel
