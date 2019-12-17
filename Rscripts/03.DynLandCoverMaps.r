library(raster)
library(tidyverse)
setwd("C:/WORK/FUNCT.NET/TOY")

time.step <- 10
scn.name <- "scn05_neutral"
landscape <- "neutral"

current.time <- as.numeric(readLines("Model/lockfile", n=1))

## Code of land-covers
bare <- 1
crop <- 2
tree <- 3
qplant <- 4
pplant <- 5
aplant <- 6

## For every time step, 
if(current.time>10){
  LAND.COVER <- raster(paste0("Model/inputlyrs/",landscape,"/land_cover", current.time-time.step, ".img"))
  
  ## BUILD FUNCTIONAL NETWORK
  PATCH.ID <- LAND.COVER
  PATCH.ID[!is.na(PATCH.ID[]) & PATCH.ID[]<tree] <-  NA
  PATCH.ID[PATCH.ID[]==tree] <-  1
  PATCH.ID <- landscapemetrics::get_patches(PATCH.ID, directions=8)[[1]]
  patches <- data.frame(id=PATCH.ID[], coordinates(PATCH.ID))
  patches.area <- filter(patches, !is.na(id)) %>% group_by(id) %>% summarize(area=length(id), xm=round(mean(x),1), ym=round(mean(y),1))
  write.table(patches.area, paste0("Model/outputs/", scn.name, "/patch.area_", current.time, ".txt"),
              quote=F, sep="\t")
  
  ## ANALYZE FUNCTIONAL NETWORK
  
  ## ???
  
  # Do some plantations in bare land
  bare.land <- which(LAND.COVER[]==bare)
  rnd.plant <- sample(bare.land, round(runif(1,0,length(bare.land))), replace=F)
  LAND.COVER[rnd.plant] <- sample(3:5, length(rnd.plant), replace=T)
  # Add some bare land, 1% of cropland
  crop <- which(LAND.COVER[]==crop)
  rnd.bare <- sample(crop, round(length(crop)*0.01), replace=F)
  LAND.COVER[rnd.bare] <- 1
  # Convert 0.5% of forest in non-productive land
  forest <- which(LAND.COVER[]==tree)
  rnd.crop <- sample(forest, round(length(forest)*0.005), replace=F)
  LAND.COVER[rnd.crop] <- 2
  
}
writeRaster(LAND.COVER, file=paste0("Model/inputlyrs/neutral/land_cover", current.time, ".img"), 
            format="HFA", datatype="INT2S", overwrite=T, NAflag=0)

