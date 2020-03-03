## Function to compute euclidean distance between patches from their X - Y coordinates
euclid.dist <- function(x, y) min(as.numeric(sp::spDists(as.matrix(x), as.matrix(y))))

## Function that finds clusters and calculate minimum Euclidean distance between pair of clusters
# r has to be a categorical map
# node.file name of the output file to report nodes list
# link.file name of the output file to report links list
build.cdg <- function(r, directions=8, node.size.th=0, node.file=NA, link.file=NA){
  
  library(raster)
  library(tidyverse)
  
  ## 1. Find patches using a 4-neigbour or a 8-neigbour rule
  ## Both NA and zero are used as background values (i.e. these values are used to separate clumps). 
  ## 'clump' function doesn't work with categorical rasters with more than 1 class:
  ## cells with any value greater than 0 are considered "land".
  r.val <- sort(unique(r[]))
  max.id <- 0
  for(i in r.val[-1]){
    sub.r <- r; sub.r[r!=i] <- 0
    clusters <- clump(sub.r, directions=directions)
    aux <- data.frame(id=clusters[], class=i, coordinates(clusters))
    aux <- filter(aux, !is.na(id)) %>% mutate(id=id+max.id)
    max.id <- max(aux$id)
    if(!exists("patch.id"))
      patch.id <- aux
    else
      patch.id <- rbind(patch.id,aux)
  }
  rm(i); rm(r.val); rm(max.id); rm(aux); rm(sub.r); rm(clusters); gc()
  
  
  ## 2. Create the data.frame with the id of the patches and the coordinates of the cells
  ## Only keep no NA cells and order by patch.id
  nodesCDG <- filter(patch.id, !is.na(id)) %>% group_by(id) %>% 
              summarise(size=length(id), xm=mean(x), ym=mean(y)) %>% filter(size > node.size.th) %>%
              mutate(patch.id=id) %>% select(-id)
  nodesCDG$node.id <- 1:nrow(nodesCDG)
  nodesCDG <- as.data.frame(nodesCDG[,c(5,4,1:3)])
  ## Save data frame in a .rdata file
  if(!is.na(node.file))
    save(nodesCDG, file=node.file)
  
 
  ## 3. For each pair of patches find the minimum distance and save a data.frame 
  ## Compute the Euclidean distance between the nodes of the network, i.e.:
  ## For each pair of nodes save in a matrix: link.id, node1, node2, dist.from1.to2
  ## I want to calculate the distance from each target node to all the other nodes
  ## except for those nodes that have already been target nodes (to not repeat calculation)
  ## So, I will calculate n-1, n-2, n-3, .... distances. The sum of n integers is (n*(n + 1))/2
  ## I first work with patch.id as these are the ones informed in PATCH.ID raster
  nnode <- nrow(nodesCDG)
  ids <- nodesCDG$patch.id
  # List with X - Y coordinates of all cells within nodes
  coord <- vector("list", nnode)
  for(i in 1:nnode)
    coord[[i]] <- filter(patch.id, id==ids[i]) %>% select(-id)
  # Matrix to store the links, i.e. distance between pair of patches
  dist.patch <- matrix(nrow=((nnode-1)*nnode)/2, ncol=4)
  dist.patch[,1] <- 1:nrow(dist.patch)
  index.ini <- 1; index.end <- nnode-1
  for(i in 1:(nnode-1)){
    print(paste0("node: ", i,  "/", nnode, ", ", round(100*i/nnode,1), "%"))
    dist.patch[index.ini:index.end,2] <- ids[i]
    dist.patch[index.ini:index.end,3] <- ids[(i+1):nnode]
    dist.patch[index.ini:index.end,4] <- 
      sapply(coord[(i+1):nnode], euclid.dist, y = coord[[i]], simplify = "array")   
    index.ini <- index.end+1
    index.end <- index.ini + length((i+2):nnode) -1
  }
  
  ## Convert to a data frame and remplace patch.id by node.id
  dist.patch <- as.data.frame(dist.patch)
  names(dist.patch) <- c("link.id", "patch1", "patch2", "dist")
  linksCDG <- left_join(dist.patch, nodesCDG[,1:2], by=c("patch1"="patch.id")) %>%
    left_join(nodesCDG[,1:2], by=c("patch2"="patch.id")) 
  linksCDG <- linksCDG[, c(1,5,6,4)]
  names(linksCDG) <- c("link.id", "node1", "node2", "dist")
  
  ## Make the graph directed by copying pairs of source-target nodes, changing their role but assigning the same distance
  linksCDG <- rbind(linksCDG,
                    data.frame(link.id=linksCDG$link.id+max(linksCDG$link.id), 
                               node1=linksCDG$node2, node2=linksCDG$node1, dist=linksCDG$dist))
                               
  ## Save it
  if(!is.na(link.file))
    save(linksCDG, file=link.file)
  
  return(list(nodesCDG, linksCDG))
  
                                 
}
