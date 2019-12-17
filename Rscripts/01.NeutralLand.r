# http://santiago.begueria.es/2010/10/generating-spatially-correlated-random-fields-with-r/
rm(list=ls())
library(gstat)
library(raster)
library(viridis)
setwd("C:/WORK/FUNCT.NET/TOY")

## Create a spatially correlated random landscape 
## Parameter 'psill' controls the range of values
## Parameter 'range' controls the spatial correlation, how similar values are spatially aggregated
xy <- expand.grid(1:100, 1:100)
names(xy) <- c('x','y')
g.dummy <- gstat(formula=z~1, locations=~x+y, dummy=T, beta=1, 
                 model=vgm(psill=0.05, range=3, model='Exp'), nmax=20)
yy <- predict(g.dummy, newdata=xy, nsim=4)
gridded(yy) = ~x+y
spplot(obj=yy[1])

## Convert continuous random landscape into a land-cover map
## Land-covers:
bare <- 1
tree <- 6
crop <- 2
qplant <- 3
pplant <- 4
aplant <- 5
# Let's plot the 4 initial maps
par(mfrow=c(2,2))
## Initialize land_cover10
land.cover <- raster(yy)
land.cover[] <- ifelse(land.cover[]<1,crop,tree)
rnd.bare <- sample(1:ncell(land.cover),round(runif(1,1,100)),replace=F)
land.cover[rnd.bare] <- bare
extent(land.cover) <- c(0,100,0,100)
plot(land.cover, col=rainbow(6), main="land")
writeRaster(land.cover, file="Model/inputlyrs/neutral/land_cover10.img", format="HFA", datatype="INT2S", overwrite=T, NAflag=0)
## Initial communities 
forest <- land.cover
forest[forest[]!=tree] <- 0
forest[forest[]==tree] <- sample(1:7, sum(forest[]==tree), replace=T, p=c(0.3,0.05,0.05,0.2,0.1,0.25,0.05))
plot(forest, col=c("grey",rainbow(7)), main="forest communities")
writeRaster(forest, file="Model/inputlyrs/neutral/initial_communities.img", format="HFA", datatype="INT2S", overwrite=T, NAflag=-1)
## Initialize land_cover0
land.cover[] <- 6
writeRaster(land.cover, file="Model/inputlyrs/neutral/land_cover0.img", format="HFA", datatype="INT2S", overwrite=T, NAflag=0)
## Initialize ecoregions
## Parameter 'germs' controls the number of seeds from which build the clusters, if these is >> than
##  'patch_classes', the space will be divided in more polygons
## Parameter 'resolution' the size of the pixels (should be 1)
## Parameter 'patch_classes' the number of classes (germs need to be equal or larger than patch_classes)
ecoreg <- NLMR::nlm_mosaicgibbs(ncol=100, nrow=100, resolution=1, germs=9, R=1, patch_classes=3, rescale=F)
plot(ecoreg, col=viridis(3), main="ecoregions")
writeRaster(ecoreg, file="Model/inputlyrs/neutral/ecoregions.img", format="HFA", datatype="INT2S", overwrite=T, NAflag=0)
# managment areas and stands
mgmt.area <- NLMR::nlm_mosaicgibbs(ncol=100, nrow=100, resolution=1, germs=3, R=1, patch_classes=3, rescale=F)
plot(mgmt.area, col=plasma(3), main="management areas")
writeRaster(mgmt.area, file="Model/inputlyrs/neutral/management_areas.img", format="HFA", datatype="INT2S", overwrite=T, NAflag=0)
land.cover[] <- 1:ncell(land.cover)
writeRaster(land.cover, file="Model/inputlyrs/neutral/stands.img", format="HFA", datatype="INT2S", overwrite=T, NAflag=0)





##### My 4x4 dummy landscape
rm(list=ls())
bare <- 1
tree <- 6
crop <- 2
qplant <- 3
pplant <- 4
aplant <- 5
# land
xyz <- data.frame(rep(1:4,4), rep(1:4,each=4), rep(tree,16))
land <- rasterFromXYZ(xyz, res=c(NA,NA), crs=NA, digits=5)
writeRaster(land, file="Model/inputlyrs/land_cover0.img", format="HFA", datatype="INT2S", overwrite=T, NAflag=0)
# forest
forest <- land
forest[forest[]<tree] <- 0
forest[forest[]==tree] <- sample(1:7, sum( forest[]==tree), replace=T, p=c(0.1,0.15,0.1,0.2,0.1,0.2,0.15))
forest[][13:16] <- 0
writeRaster(forest, file="Model/inputlyrs/initial_communities.img", format="HFA", datatype="INT2S", overwrite=T, NAflag=0)
# ecoregions
ecoreg <- NLMR::nlm_mosaicgibbs(ncol=4, nrow=4, resolution=1, germs=3, R=1, patch_classes=3, rescale=F)
extent(ecoreg) <- extent(forest)
writeRaster(ecoreg, file="Model/inputlyrs/ecoregions.img", format="HFA", datatype="INT2S", overwrite=T, NAflag=-1)
# managment areas
margin <- 1
productive <- 2
reserve <- 3
xyz <- data.frame(rep(1:4,4), rep(1:4,each=4), c(rep(margin,4), rep(productive,8), rep(reserve,3), productive) )
mgmt.area <- rasterFromXYZ(xyz, res=c(NA,NA), crs=NA, digits=5)
writeRaster(mgmt.area, file="Model/inputlyrs/management_areas.img", format="HFA", datatype="INT2S", overwrite=T, NAflag=0)
xyz <- data.frame(rep(1:4,4), rep(1:4,each=4), 1:16)
stands <- rasterFromXYZ(xyz, res=c(NA,NA), crs=NA, digits=5)
writeRaster(stands, file="Model/inputlyrs/stands.img", format="HFA", datatype="INT2S", overwrite=T, NAflag=0)
# plot the 4 initil maps
par(mfrow=c(2,2))
plot(land, col=rainbow(1), main="land")
plot(forest, col=rainbow(7), main="forest communities")
plot(ecoreg, col=viridis(3), main="ecoregions")
plot(mgmt.area, col=plasma(3), main="management areas")
# sucessive land-cover maps
xyz <- data.frame(rep(1:4,4), rep(1:4,each=4), c(rep(bare,2), rep(crop,2), rep(tree,12)) )
land <- rasterFromXYZ(xyz, res=c(NA,NA), crs=NA, digits=5)
writeRaster(land, file="Model/inputlyrs/land_cover10.img", format="HFA", datatype="INT2S", overwrite=T, NAflag=0)
xyz <- data.frame(rep(1:4,4), rep(1:4,each=4), c(rep(bare,2), rep(crop,6),  rep(tree,8)) )
land <- rasterFromXYZ(xyz, res=c(NA,NA), crs=NA, digits=5)
writeRaster(land, file="Model/inputlyrs/land_cover20.img", format="HFA", datatype="INT2S", overwrite=T, NAflag=0)
xyz <- data.frame(rep(1:4,4), rep(1:4,each=4), c(rep(bare,2), rep(crop,10),  rep(tree,4)) )
land <- rasterFromXYZ(xyz, res=c(NA,NA), crs=NA, digits=5)
writeRaster(land, file="Model/inputlyrs/land_cover30.img", format="HFA", datatype="INT2S", overwrite=T, NAflag=0)
xyz <- data.frame(rep(1:4,4), rep(1:4,each=4), c(rep(bare,2), rep(crop,7), qplant, qplant, crop,  rep(tree,4)) )
land <- rasterFromXYZ(xyz, res=c(NA,NA), crs=NA, digits=5)
writeRaster(land, file="Model/inputlyrs/land_cover40.img", format="HFA", datatype="INT2S", overwrite=T, NAflag=0)
writeRaster(land, file="Model/inputlyrs/land_cover50.img", format="HFA", datatype="INT2S", overwrite=T, NAflag=0)
xyz <- data.frame(rep(1:4,4), rep(1:4,each=4), c(rep(bare,2), rep(pplant,2), rep(crop,5), qplant, qplant, crop, rep(tree,4)) )
land <- rasterFromXYZ(xyz, res=c(NA,NA), crs=NA, digits=5)
writeRaster(land, file="Model/inputlyrs/land_cover60.img", format="HFA", datatype="INT2S", overwrite=T, NAflag=0)
xyz <- data.frame(rep(1:4,4), rep(1:4,each=4), c(bare, crop, rep(pplant,2), rep(crop,5), qplant, qplant, crop, rep(tree,4)) )
land <- rasterFromXYZ(xyz, res=c(NA,NA), crs=NA, digits=5)
writeRaster(land, file="Model/inputlyrs/land_cover70.img", format="HFA", datatype="INT2S", overwrite=T, NAflag=0)
xyz <- data.frame(rep(1:4,4), rep(1:4,each=4), c(bare, crop, rep(pplant,2), rep(crop,5), qplant, qplant, crop, rep(tree,3), aplant) )
land <- rasterFromXYZ(xyz, res=c(NA,NA), crs=NA, digits=5)
writeRaster(land, file="Model/inputlyrs/land_cover80.img", format="HFA", datatype="INT2S", overwrite=T, NAflag=0)
writeRaster(land, file="Model/inputlyrs/land_cover90.img", format="HFA", datatype="INT2S", overwrite=T, NAflag=0)
