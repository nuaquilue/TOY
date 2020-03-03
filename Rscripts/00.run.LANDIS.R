rm(list=ls())
setwd("C:/WORK/FUNCT.NET/TOY/Model")

scn.name <- "scn05_neutral"
landscape <- "neutral"

## Modify Scenario file
scn.file  <- readLines("scenario_template.txt")
scn.file <- gsub("landscape", landscape, scn.file)
writeLines(scn.file, con="scenario.txt")

## Modify Biomass Succession file
biomass.file  <- readLines("extensions/biomass_succession_template.txt")
biomass.file <- gsub("landscape", landscape, biomass.file)
writeLines(biomass.file, con="extensions/biomass_succession.txt")

## Modify Harvest file
harvest.file  <- readLines("extensions/harvest_template.txt")
harvest.file <- gsub("landscape", landscape, harvest.file)
harvest.file <- gsub("scn.name", scn.name, harvest.file)
writeLines(harvest.file, con="extensions/harvest.txt")

## Modify LandUse file
landuse.file  <- readLines("extensions/land_use_template.txt")
landuse.file <- gsub("landscape", landscape, landuse.file)
landuse.file <- gsub("scn.name", scn.name, landuse.file)
writeLines(landuse.file, con="extensions/land_use.txt")

## Modify Output Biomass file
output.file  <- readLines("extensions/output_biomass_template.txt")
output.file <- gsub("scn.name", scn.name, output.file)
writeLines(output.file, con="extensions/output_biomass.txt")

## Run LANDIS 
shell("RunLANDIS.bat", intern=F)

## Copy scneario and landis log files in the outputs folder 
## Also copy or remove innecessary automatic outputs
file.rename("scenario.txt", paste0("outputs/", scn.name, "/scenario.txt"))
file.rename("Landis-log.txt", paste0("outputs/", scn.name, "/Landis-log.txt"))
for(year in seq(10,90,10))
  file.rename(paste0("biomass-succession/biomass-anpp-", year,".img"), 
              paste0("outputs/", scn.name, "/biomass-succession/biomass-anpp-", year,".img"))
file.rename("Biomass-succession-log.csv", 
            paste0("outputs/", scn.name, "/biomass-succession/Biomass-succession-log.csv"))
unlink("Metadata", recursive = T) 
unlink("biomass-succession", recursive = T)
file.remove("lockfile")

    # 
    # wdir <- "C:/WORK/REDEFINE/LANDIS/NuTraining/"
    # for(rep in c("rep1", "rep2")){
    #   subdir <- paste0(wdir, "Chapter_6_", rep, "/")
    #   setwd(subdir)
    #   shell(paste0(subdir, "SimpleBatchFile.bat"))
    #   closeAllConnections()
    # }