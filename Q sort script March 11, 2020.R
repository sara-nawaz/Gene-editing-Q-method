#Load "qmethod" and "ggplot2" packages
library(qmethod)
library(ggplot2)
setwd("/Users/saranawaz/Desktop/Stuff/PhD/Research projects/Genome BC Perceptions of Gene Editing in Agriculture/QMethod/data files")

### load cattle dataset #########################################################################

#Select cattle dataset
data.cattle <- read.csv(file = 'Qcattle_check.csv', header=T, sep=",", row.names = 1)



### determine # of cattle factors ###############################################################

#Conduct principle component analysis
data.cattle.pca <- prcomp(data.cattle, center=TRUE, scale.=TRUE)

#Note which factors have eigenvalues (EVs) >1
summary(data.cattle.pca)

#Note where slope of plotted EVs flattens out
screeplot(data.cattle.pca, type = "l", npcs = 15, main = "Screeplot of all PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

#Also we can see the cumulative variance explained by these PCs
cumpro <- cumsum(data.cattle.pca$sdev^2 / sum(data.cattle.pca$sdev^2))
plot(cumpro[0:9], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 3, col="blue", lty=5)
abline(h = 0.7, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC3"),
       col=c("blue"), lty=5, cex=0.6)

#Use this analysis to determine number of factors to include in Q analysis below.

### conduct basic Q analysis ######################################################################

#Calculate results, including setting number of factors (**based on PCA conducted)
results.cattle = qmethod(data.cattle, nfactors=3, rotation="varimax", forced = TRUE)

#Print results
results.cattle$qdc
format(results.cattle$qdc, digits = 1, nsmall = 2)

#Plot results
plot(results.cattle,
     colours = c("#e41a1c",  "#377eb8", "#4daf4a"),
     xlim=c(-2.5, 2.5), 
     cex=1.1, cex.axis=0.8, cex.main=1,
     leg.pos="bottomleft")

### apply Crib Method #########################################################################

#Quick overview: Sample q chart for each factor
#Goal here is to quickly understand what an indicative set of responses looks like for each factor
results.cattle$zsc_n

#Statement scores for Factor 1, organized highest to lowest
#Goal here is to see what statements are highest for Factor 1
scores.cattle <- cbind(round(results.cattle$zsc, digits=2), results.cattle$zsc_n)
nfactors.cattle <- ncol(results.cattle$zsc)
col.order.cattle <- as.vector(rbind(1:nfactors.cattle, (1:nfactors.cattle)+nfactors.cattle))
scores.cattle <- scores.cattle[col.order.cattle]
scores.cattle[order(scores.cattle$zsc_f1, decreasing = T), ]

#Statement scores for Factor 2, organized highest to lowest
#Goal here is to see what statements are highest for Factor 2
scores.cattle[order(scores.cattle$zsc_f2, decreasing = T), ]

#Statement scores for Factor 3, organized highest to lowest
#Goal here is to see what statements are highest for Factor 3
scores.cattle[order(scores.cattle$zsc_f3, decreasing = T), ]









### load tomato dataset #########################################################################

#Select tomato dataset
data.tomato <- read.csv(file = 'Qtomato_check.csv', header=T, sep=",", row.names = 1)

### determine # of tomato factors ###############################################################

#Conduct principle component analysis
data.tomato.pca <- prcomp(data.tomato, center=TRUE, scale.=TRUE)

#Note which factors have eigenvalues (EVs) >1
summary(data.tomato.pca)

#Note where slope of plotted EVs flattens out
screeplot(data.tomato.pca, type = "l", npcs = 15, main = "Screeplot of all PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

#Also we can see the cumulative variance explained by these PCs
cumpro <- cumsum(data.tomato.pca$sdev^2 / sum(data.tomato.pca$sdev^2))
plot(cumpro[0:9], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 3, col="blue", lty=5)
abline(h = 0.7, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC3"),
       col=c("blue"), lty=5, cex=0.6)

#Use this analysis to determine number of factors to include in Q analysis below.

### conduct basic Q analysis ######################################################################

#Calculate results, including setting number of factors (**based on PCA conducted)
results.tomato = qmethod(data.tomato, nfactors=3, rotation="varimax", forced = TRUE)

#Print results
results.tomato$qdc
format(results.tomato$qdc, digits = 1, nsmall = 2)

#Plot results
plot(results.tomato,
     colours = c("#e41a1c",  "#377eb8", "#4daf4a"),
     xlim=c(-2.5, 2.5), 
     cex=1.1, cex.axis=0.8, cex.main=1,
     leg.pos="bottomleft")

### apply Crib Method #########################################################################

#Quick overview: Sample q chart for each factor
#Goal here is to quickly understand what an indicative set of responses looks like for each factor
results.tomato$zsc_n

#Statement scores for Factor 1, organized highest to lowest
#Goal here is to see what statements are highest for Factor 1
scores.tomato <- cbind(round(results.tomato$zsc, digits=2), results.tomato$zsc_n)
col.order.tomato <- as.vector(rbind(1:nfactors, (1:nfactors)+nfactors))
scores.tomato <- scores.tomato[col.order]
scores.tomato[order(scores.tomato$zsc_f1, decreasing = T), ]

#Statement scores for Factor 2, organized highest to lowest
#Goal here is to see what statements are highest for Factor 2
scores.tomato[order(scores.tomato$zsc_f2, decreasing = T), ]

#Statement scores for Factor 3, organized highest to lowest
#Goal here is to see what statements are highest for Factor 3
scores.tomato[order(scores.tomato$zsc_f3, decreasing = T), ]





### load fruit flies (ff) dataset #########################################################################

#Select ff dataset
data.ff <- read.csv(file = 'Qfruitfly_check.csv', header=T, sep=",", row.names = 1)


### determine # of ff factors ###############################################################

#Conduct principle component analysis
data.ff.pca <- prcomp(data.ff, center=TRUE, scale.=TRUE)

#Note which factors have eigenvalues (EVs) >1
summary(data.ff.pca)

#Note where slope of plotted EVs flattens out
screeplot(data.ff.pca, type = "l", npcs = 15, main = "Screeplot of all PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

#Also we can see the cumulative variance explained by these PCs
cumpro <- cumsum(data.ff.pca$sdev^2 / sum(data.ff.pca$sdev^2))
plot(cumpro[0:9], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 3, col="blue", lty=5)
abline(h = 0.76, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC3"),
       col=c("blue"), lty=5, cex=0.6)

#Use this analysis to determine number of factors to include in Q analysis below.

### conduct basic Q analysis ######################################################################

#Calculate results, including setting number of factors (**based on PCA conducted)
results.ff = qmethod(data.ff, nfactors=3, rotation="varimax", forced = TRUE)

#Print results
results.ff$qdc
format(results.ff$qdc, digits = 1, nsmall = 2)

#Plot results
plot(results.ff,
     colours = c("#e41a1c",  "#377eb8", "#4daf4a"),
     xlim=c(-2.5, 2.5), 
     cex=1.1, cex.axis=0.8, cex.main=1,
     leg.pos="bottomleft")

### apply Crib Method #########################################################################

#Quick overview: Sample q chart for each factor
#Goal here is to quickly understand what an indicative set of responses looks like for each factor
results.ff$zsc_n

#Statement scores for Factor 1, organized highest to lowest
#Goal here is to see what statements are highest for Factor 1
scores.ff <- cbind(round(results.ff$zsc, digits=2), results.ff$zsc_n)
nfactors.ff <- ncol(results.ff$zsc)
col.order.ff <- as.vector(rbind(1:nfactors, (1:nfactors)+nfactors))
scores.ff <- scores.ff[col.order]
scores.ff[order(scores.ff$zsc_f1, decreasing = T), ]

#Statement scores for Factor 2, organized highest to lowest
#Goal here is to see what statements are highest for Factor 2
scores.ff[order(scores.ff$zsc_f2, decreasing = T), ]

#Statement scores for Factor 3, organized highest to lowest
#Goal here is to see what statements are highest for Factor 3
scores.ff[order(scores.ff$zsc_f3, decreasing = T), ]








### load wheat dataset #########################################################################

#Select wheat dataset
data.wheat <- read.csv(file = 'Qwheat_check.csv', header=T, sep=",", row.names = 1)


### determine # of wheat factors ###############################################################

#Conduct principle component analysis
data.wheat.pca <- prcomp(data.wheat, center=TRUE, scale.=TRUE)

#Note which factors have eigenvalues (EVs) >1
summary(data.wheat.pca)

#Note where slope of plotted EVs flattens out
screeplot(data.wheat.pca, type = "l", npcs = 15, main = "Screeplot of all PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

#Also we can see the cumulative variance explained by these PCs
cumpro <- cumsum(data.wheat.pca$sdev^2 / sum(data.wheat.pca$sdev^2))
plot(cumpro[0:9], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 3, col="blue", lty=5)
abline(h = 0.76, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC3"),
       col=c("blue"), lty=5, cex=0.6)

#Use this analysis to determine number of factors to include in Q analysis below.

### conduct basic Q analysis ######################################################################

#Calculate results, including setting number of factors (**based on PCA conducted)
results.wheat = qmethod(data.wheat, nfactors=3, rotation="varimax", forced = TRUE)

#Print results
results.wheat$qdc
format(results.wheat$qdc, digits = 1, nsmall = 2)

#Plot results
plot(results.wheat,
     colours = c("#e41a1c",  "#377eb8", "#4daf4a"),
     xlim=c(-2.5, 2.5), 
     cex=1.1, cex.axis=0.8, cex.main=1,
     leg.pos="bottomleft")

### apply Crib Method #########################################################################

#Quick overview: Sample q chart for each factor
#Goal here is to quickly understand what an indicative set of responses looks like for each factor
results.wheat$zsc_n

#Statement scores for Factor 1, organized highest to lowest
#Goal here is to see what statements are highest for Factor 1
scores.wheat <- cbind(round(results.wheat$zsc, digits=2), results.wheat$zsc_n)
nfactors.wheat <- ncol(results.wheat$zsc)
col.order.wheat <- as.vector(rbind(1:nfactors, (1:nfactors)+nfactors))
scores.wheat <- scores.wheat[col.order]
scores.wheat[order(scores.wheat$zsc_f1, decreasing = T), ]

#Statement scores for Factor 2, organized highest to lowest
#Goal here is to see what statements are highest for Factor 2
scores.wheat[order(scores.wheat$zsc_f2, decreasing = T), ]

#Statement scores for Factor 3, organized highest to lowest
#Goal here is to see what statements are highest for Factor 3
scores.wheat[order(scores.wheat$zsc_f3, decreasing = T), ]

