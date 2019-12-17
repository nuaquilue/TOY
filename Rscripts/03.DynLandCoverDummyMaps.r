setwd("C:/WORK/FUNCT.NET/TOY")

library(raster)

timestep <- as.numeric(readLines("C:/WORK/FUNCT.NET/TOY/Model/lockfile", n=1))

## Code of land-covers
bare <- 1
tree <- 6
crop <- 2
qplant <- 3
pplant <- 4
aplant <- 5

if(timestep==10)
  xyz <- data.frame(rep(1:4,4), rep(1:4,each=4), c(rep(bare,2), rep(crop,2), rep(tree,12)) )
if(timestep==20)
  xyz <- data.frame(rep(1:4,4), rep(1:4,each=4), c(rep(bare,2), rep(crop,6),  rep(tree,8)) )
if(timestep==30)
  xyz <- data.frame(rep(1:4,4), rep(1:4,each=4), c(aplant, bare, rep(crop,10),  rep(tree,4)) )
if(timestep==40)
  xyz <- data.frame(rep(1:4,4), rep(1:4,each=4), c(aplant, bare, rep(crop,7), qplant, qplant, crop,  rep(tree,4)) )
if(timestep==50)
  xyz <- data.frame(rep(1:4,4), rep(1:4,each=4), c(aplant, bare, rep(crop,2), bare, rep(crop,4), qplant, qplant, crop,  rep(tree,4)) )
if(timestep==60)
  xyz <- data.frame(rep(1:4,4), rep(1:4,each=4), c(aplant, bare, rep(pplant,2), bare, rep(crop,4), qplant, qplant, crop, rep(tree,4)) )
if(timestep==70)
  xyz <- data.frame(rep(1:4,4), rep(1:4,each=4), c(aplant, crop, rep(pplant,2), bare, rep(crop,4), qplant, qplant, crop, rep(tree,4)) )
if(timestep==80)
  xyz <- data.frame(rep(1:4,4), rep(1:4,each=4), c(aplant, crop, rep(pplant,2), bare, rep(crop,4), qplant, qplant, crop, rep(tree,3), aplant) )
if(timestep==90)
  xyz <- data.frame(rep(1:4,4), rep(1:4,each=4), c(aplant, crop, rep(pplant,2), bare, rep(crop,5), qplant, crop, rep(tree,3), aplant) )

land <- rasterFromXYZ(xyz, res=c(NA,NA), crs=NA, digits=5)
writeRaster(land, file=paste0("Model/inputlyrs/dummy/land_cover",timestep,".img"), 
            format="HFA", datatype="INT2S", overwrite=T, NAflag=0)


  # ## Management areas
  # margin <- 1
  # productive <- 2
  # reserve <- 3
  # xyz <- data.frame(rep(1:4,4), rep(1:4,each=4), c(rep(margin,4), rep(productive,8), rep(reserve,3), productive) )
  # mgmt.area <- rasterFromXYZ(xyz, res=c(NA,NA), crs=NA, digits=5)
  # writeRaster(mgmt.area, file=paste0("Model/inputlyrs/management_areas",timestep,".img"), 
  #             format="HFA", datatype="INT2S", overwrite=T, NAflag=0)
