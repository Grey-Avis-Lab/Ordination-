#Ordination plotting created by Keith McGuinness (https://www.youtube.com/watch?v=paO9-PLXwD0) and edited by Evan Warburton 3/27/23

library(vegan)
library(gclus)
library(tidyverse)
library(ggplot2)

spe <- read.csv("C:/Users/ewarb/OneDrive/Documents/islandfungi2/metadata_funguild.csv")
spe.only <- spe[,-1]
rownames(spe.only) <- spe[,1]

spe.nmds <- metaMDS(spe.only)
spe.nmds$stress
par(mfrow=c(1,1))
plot(spe.nmds, type="t", main=paste("SPE NMDS/Bray-Stress =", round(spe.nmds$stress,3)))

#Shepard plot and goodness of fit
par(mfrow=c(1,2))
stressplot(spe.nmds, main="SPE Shepard plot")
gof <- goodness(spe.nmds)
max.gof <- max(gof)
point.size <- 1/ max.gof
sit.sc <- scores(spe.nmds)
plot(spe.nmds, type="n", main="SPE Goodness of fit")
points(sit.sc, pch=21, cex=gof*point.size)
text(sit.sc, row.names(spe), pos=3, cex=0.7)

#Plot the NMDS: need to add group colors
par(mfrow=c(1,1))
sit.sc <- scores(spe.nmds)
col.group <- as.factor(spe[,1])
x.coord <- sit.sc[]
y.coord <- sit.sc
plot(x.coord, y.coord, main="NMDS/Bray on SPE", xlab="NMDS1", ylab="NMDS2", pch=19, col=col.group)
text(sit.sc,row.names(spe),pos=4,cex=0.7)

#Ordination by Sites
sit.sc <- scores(spe.nmds$species)
sit.sc %>%
  as_tibble(rownames="sites") %>%
  ggplot(aes(x=MDS1,y=MDS2, label=sites))+
  geom_point()+geom_text(hjust=0, vjust=0)+
  labs(title='Ordination by Island Sites')+
  xlab('NMDS1')+
  ylab('NMDS2')

#Ordination by Island
sit.sc <- scores(spe.nmds$species)
sit.sc %>%
  as_tibble(rownames="sites") %>%
  ggplot(aes(x=MDS1,y=MDS2, color=sites))+
  geom_point(size=3)+
  labs(title='Ordination by Island')+
  xlab('NMDS1')+
  ylab('NMDS2')
