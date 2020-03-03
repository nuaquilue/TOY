rm(list=ls())
library(landscapemetrics)
library(NLMR)
library(sp)
library(raster)
library(viridis)
library(tidyverse)
setwd("C:/WORK/FUNCT.NET/TOY")
source("rscripts/04.build.cdg.r")

## Dimensions
ncol <- 101
nrow <- 101


################ Initialize a 100x100 landscape with 2 land-cover types using: ################

## 1. A 'random cluster' approach 
LANDrc1 <- nlm_randomcluster(ncol, nrow, resolution=100, p=0.4, ai=c(0.5, 0.5), neighbourhood=8, rescale=F)
LANDrc2 <- nlm_randomcluster(ncol, nrow, resolution=100, p=0.28, ai=c(0.5, 0.5), neighbourhood=8, rescale=F)

## 2. A fractional Brownian motion, reclassified at 2 categories
LANDfbm1 <- nlm_fbm(ncol, nrow, resolution=100, fract_dim=0.9)
LANDfbm1 <- landscapetools::util_classify(LANDfbm1, weighting = c(0.5, 0.5))
LANDfbm2 <- nlm_fbm(ncol, nrow, resolution=100, fract_dim=0.2)
LANDfbm2 <- landscapetools::util_classify(LANDfbm2, weighting = c(0.5, 0.5))

## 3. A random neighborhood
LANDngh1 <- nlm_neigh(ncol, nrow, resolution=100, p_neigh=0.99, p_empty=0.01, categories=2, neighbourhood=8, rescale=F)
LANDngh2 <- nlm_neigh(ncol, nrow, resolution=100, p_neigh=0.5, p_empty=0.5, categories=2, neighbourhood=8, rescale=F)   

## Count and plot the initial maps
table(LANDrc1[]-1); table(LANDrc2[]-1); table(LANDfbm1[]-1); table(LANDfbm2[]-1); table(LANDngh1[]); table(LANDngh2[])
par(mfrow=c(3,2))
plot(LANDrc1, legend=F); plot(LANDrc2, legend=F) 
plot(LANDfbm1, legend=F); plot(LANDfbm2, legend=F)
plot(LANDngh1, legend=F); plot(LANDngh2, legend=F)



################## Initialize a 100x100 landscape with 2 land-cover types (forest vs. non-forest)
################ and stands or patches in forest areas, using: ################

## 4. A Random cluster approach with more categories
LANDrnd1 <- nlm_randomcluster(ncol, nrow, resolution=100, p=0.4, ai=c(0.4, 0.3, 0.3), neighbourhood=8, rescale=F)
LANDrnd2 <- nlm_randomcluster(ncol, nrow, resolution=100, p=0.28, ai=c(0.4, 0.3, 0.3), neighbourhood=8, rescale=F)
LANDrnd3 <- nlm_randomcluster(ncol, nrow, resolution=100, p=0.4, ai=c(0.35, 0.05, 0.25, 0.15, 0.2), neighbourhood=8, rescale=F)
LANDrnd4 <- nlm_randomcluster(ncol, nrow, resolution=100, p=0.28, ai=c(0.35, 0.05, 0.25, 0.15, 0.2), neighbourhood=8, rescale=F)
LANDrnd5 <- nlm_randomcluster(ncol, nrow, resolution=100, p=0.4, ai=c(0.2, 0.1, 0.05, 0.05, 0.25, 0.15, 0.2), neighbourhood=8, rescale=F)
LANDrnd6 <- nlm_randomcluster(ncol, nrow, resolution=100, p=0.28, ai=c(0.2, 0.1, 0.05, 0.05, 0.25, 0.15, 0.2), neighbourhood=8, rescale=F)
# Count and plot
table(LANDrnd1[]-1); table(LANDrnd2[]-1); table(LANDrnd3[]-1); table(LANDrnd4[]-1); table(LANDrnd5[]-1); table(LANDrnd6[]-1)
par(mfrow=c(3,2))
# plot(LANDrnd1-1, legend=F, col=c("grey90", rep("chartreuse4", 2))) 
# plot(LANDrnd2-1, legend=F, col=c("grey90", rep("chartreuse4", 2)))
# plot(LANDrnd3-1, legend=F, col=c("grey90", rep("chartreuse4", 4))) 
# plot(LANDrnd4-1, legend=F, col=c("grey90", rep("chartreuse4", 4))) 
plot(LANDrnd1-1, legend=T, col=c("grey90", viridis(2))) 
plot(LANDrnd2-1, legend=T, col=c("grey90", viridis(2))) 
plot(LANDrnd3-1, legend=T, col=c("grey90", viridis(4))) 
plot(LANDrnd4-1, legend=T, col=c("grey90", viridis(4))) 
plot(LANDrnd5-1, legend=T, col=c("grey90", rainbow(6))) 
plot(LANDrnd6-1, legend=T, col=c("grey90", rainbow(6))) 


## 5. Use first a midpoint displacement with reclassification to determine forest vs non-forest
## This approach returns a true 50% of occupancy!
LANDmpd1 <- nlm_mpd(ncol, nrow, resolution=100, roughness=0.65, rand_dev=1, rescale = F, verbose = F)
LANDmpd1 <- landscapetools::util_classify(LANDmpd1, weighting = c(0.5, 0.5))
LANDmpd2 <- nlm_mpd(ncol, nrow, resolution=100, roughness=0.98, rand_dev=2, rescale = F, verbose = F)
LANDmpd2 <- landscapetools::util_classify(LANDmpd2, weighting = c(0.5, 0.5))
table(LANDmpd1[]-1); table(LANDmpd2[]-1)
par(mfrow=c(1,2))
plot(LANDmpd1, legend=F); plot(LANDmpd2, legend=F) 

## Now, Use the Random cluster approach to split the forest area in forest stands.
stands <- nlm_randomcluster(ncol, nrow, resolution=100, p=0.28, ai=c(0.6, 0.4), neighbourhood=8, rescale=F)
LANDmpd1.s1 <- (LANDmpd1-1)*stands
LANDmpd2.s1 <- (LANDmpd2-1)*stands
stands <- nlm_randomcluster(ncol, nrow, resolution=100, p=0.28, ai=c(0.5, 0.3, 0.1, 0.1), neighbourhood=8, rescale=F)
LANDmpd1.s2 <- (LANDmpd1-1)*stands
LANDmpd2.s2 <- (LANDmpd2-1)*stands
stands <- nlm_randomcluster(ncol, nrow, resolution=100, p=0.28, ai=c(0.25, 0.1, 0.15, 0.3, 0.1, 0.1), neighbourhood=8, rescale=F)
LANDmpd1.s3 <- (LANDmpd1-1)*stands
LANDmpd2.s3 <- (LANDmpd2-1)*stands
# Count and plot
table(LANDmpd1.s1[]); table(LANDmpd2.s1[]); table(LANDmpd1.s2[]); table(LANDmpd2.s2[]); table(LANDmpd1.s3[]); table(LANDmpd2.s3[])
par(mfrow=c(3,2))
plot(LANDmpd1.s1, legend=T, col=c("grey90", viridis(2))) 
plot(LANDmpd2.s1, legend=T, col=c("grey90", viridis(2)))
plot(LANDmpd1.s2, legend=T, col=c("grey90", viridis(4))) 
plot(LANDmpd2.s2, legend=T, col=c("grey90", viridis(4)))
plot(LANDmpd1.s3, legend=T, col=c("grey90", rainbow(6))) 
plot(LANDmpd2.s3, legend=T, col=c("grey90", rainbow(6))) 


## Finally, use the Random cluster approach to split the forest area in forest stands,
## and add some stochasticity to add "salt-and-pepper" effect.
stands <- nlm_randomcluster(ncol, nrow, resolution=100, p=0.28, ai=c(0.6, 0.4), neighbourhood=8, rescale=F)
LANDmpd1.s4 <- (LANDmpd1-1)*stands*sample(c(1,3), ncell(stands), replace = T, prob=c(0.7,0.3))
LANDmpd1.s4 <- subs(LANDmpd1.s4, data.frame(from=c(0,1,2,3,6), to=0:4))
LANDmpd2.s4 <- (LANDmpd2-1)*stands*sample(c(1,3), ncell(stands), replace = T, prob=c(0.7,0.3))
LANDmpd2.s4 <- subs(LANDmpd2.s4, data.frame(from=c(0,1,2,3,6), to=0:4))
LANDmpd1.s5 <- LANDmpd1.s2*sample(c(1,2), ncell(stands), replace = T, prob=c(0.3,0.7))
LANDmpd1.s5 <- subs(LANDmpd1.s5, data.frame(from=c(0,1,2,3,4,6,8), to=0:6))
LANDmpd2.s5 <- LANDmpd2.s2*sample(c(1,2), ncell(stands), replace = T, prob=c(0.3,0.7))
LANDmpd2.s5 <- subs(LANDmpd2.s5, data.frame(from=c(0,1,2,3,4,6,8), to=0:6))
LANDmpd1.s6 <- LANDmpd1.s3*sample(c(1,5), ncell(stands), replace = T, prob=c(0.7,0.3))
LANDmpd1.s6 <- subs(LANDmpd1.s6, data.frame(from=c(0,1,2,3,4,5,10,15,20), to=0:8))
LANDmpd2.s6 <- LANDmpd2.s3*sample(c(1,5), ncell(stands), replace = T, prob=c(0.7,0.3))
LANDmpd2.s6 <- subs(LANDmpd2.s6, data.frame(from=c(0,1,2,3,4,5,10,15,20), to=0:8))
# Count and plot
table(LANDmpd1.s4[]); table(LANDmpd2.s4[]); table(LANDmpd1.s5[]); table(LANDmpd2.s5[]);  table(LANDmpd1.s6[]); table(LANDmpd2.s6[])
par(mfrow=c(3,2))
plot(LANDmpd1.s4, legend=T, col=c("grey90", viridis(4))) 
plot(LANDmpd2.s4, legend=T, col=c("grey90", viridis(4)))
plot(LANDmpd1.s5, legend=T, col=c("grey90", rainbow(6))) 
plot(LANDmpd2.s5, legend=T, col=c("grey90", rainbow(6))) 
plot(LANDmpd1.s6, legend=T, col=c("grey90", rainbow(6))) 
plot(LANDmpd2.s6, legend=T, col=c("grey90", rainbow(8))) 




#### Graph building will depend on forest communities, and tree species dispersal capacity
r <- LANDrnd3-1; directions=8; node.size.th=0
plot(r, legend=T, col=c("grey90", viridis(4))) 
CDG <- build.cdg(r, directions, node.size.th) #, "DataOut/NodesCDG_LANDrnd1.rdata")
nodesCDG <- CDG[[1]]
linksCDG <- CDG[[2]]
## Build the graph 
cdg <- graph(t(linksCDG[sample(1:nrow(linksCDG), nrow(linksCDG)^0.5,replace=F),2:3]), directed=T)
## The spatial layout: To have the nodes positioned at their centroid, 
## create a normalized matrix with the coordinates
layout <- matrix(c(nodesCDG$xm, nodesCDG$ym), nc=2)
layout <- layout/max(layout)
plot(cdg, layout=layout, margin=c(0.1,0.1,0.1,0.1),
     vertex.label=NA, #vertex.color=node.colors[node.score], 
     vertex.frame.color=NA, vertex.size=log(nodesCDG$size),
     edge.arrow.size=0, edge.width=0.1, edge.color="grey80")


