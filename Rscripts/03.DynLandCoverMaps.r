library(raster)
setwd("C:/WORK/FUNCT.NET/TOY")

time.step <- 10
current.time <- as.numeric(readLines("Model/lockfile", n=1))

## Code of land-covers
bare <- 1
tree <- 6
crop <- 2
qplant <- 3
pplant <- 4
aplant <- 5

## For every time step, 
if(current.time>10){
  land.cover <- raster(paste0("Model/inputlyrs/neutral/land_cover", current.time-time.step, ".img"))
  # Do some plantations in bare land
  bare.land <- which(land.cover[]==bare)
  rnd.plant <- sample(bare.land, round(runif(1,0,length(bare.land))), replace=F)
  land.cover[rnd.plant] <- sample(3:5, length(rnd.plant), replace=T)
  # Add some bare land, 1% of cropland
  crop <- which(land.cover[]==crop)
  rnd.bare <- sample(crop, round(length(crop)*0.01), replace=F)
  land.cover[rnd.bare] <- 1
  # Convert 0.5% of forest in non-productive land
  forest <- which(land.cover[]==tree)
  rnd.crop <- sample(forest, round(length(forest)*0.005), replace=F)
  land.cover[rnd.crop] <- 2
  
}
writeRaster(land.cover, file=paste0("Model/inputlyrs/neutral/land_cover", current.time, ".img"), 
            format="HFA", datatype="INT2S", overwrite=T, NAflag=0)

