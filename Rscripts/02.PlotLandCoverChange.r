rm(list=ls())
library(raster)
library(viridis)
setwd("C:/WORK/FUNCT.NET/TOY")

## Scenario
scn.name <- "scn05_neutral"
landscape <- "neutral"

## Initial forest communities and ecoregions
par(mfrow=c(1,2))
eco <- raster(paste0("Model/inputlyrs/", landscape, "/ecoregions.img"))
plot(eco, col=viridis(3), main="Ecoregions")
mgmt.area <- raster(paste0("Model/inputlyrs/", landscape, "/management_areas.img"))
plot(mgmt.area, col=plasma(3), main="Management areas")

## Biomass maps
species <- c("abiebals", "acerrubr", "acersacc", "betualle", "betupapy", "fraxamer", "piceglau", "pinubank", 
             "pinuresi", "pinustro", "poputrem", "querelli", "querrubr", "thujocci", "tiliamer", "tsugcana")
for(year in seq(10,90,10)){
  biom <- raster(paste0("Model/outputs/",scn.name,"/biomass-succession/biomass-abiebals-",year,".img"))
    # print(paste0("y", year, " - ", species[1], ":")); print(biom[]/1000)
  for(spp in species[-1]){
    aux <- raster(paste0("Model/outputs/",scn.name,"/biomass-succession/biomass-",spp,"-",year,".img"));  biom <- biom + aux
   # print(paste0("y", year, " - ", spp, ":")); print(aux[]/1000)
  }
  # print(paste("year", year, ":"));   print(biom[]/1000)
  land <- raster(paste0("Model/inputlyrs/",landscape,"/land_cover",year,".img"))
  plot(land, col=rainbow(6), main=paste0("Biomass y", year))
  plot(biom/1000, col=c("grey", viridis(20)), main=paste0("Biomass y", year))               
}

