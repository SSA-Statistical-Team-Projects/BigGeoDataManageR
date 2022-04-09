### a quick script to facililate pulling the night time light data from the EOG server and crop out Africa proportion from the raster

##### a function do this
library(dplyr)



crop_raster_to_africa <- function(raster_folder = "//esapov/esapov/ALL/Energy",
                                  numCores = 15){

  sf::sf_use_s2(FALSE)
  ##read in africa shapefile
  africa_shp <- sf::st_read(dsn = "//esapov/esapov/ALL/Boundaries",
                            layer = "Africa_Boundaries")

  africa_shp <- africa_shp %>% sf::st_bbox() %>% sf::st_as_sfc()
  africa_shp <- africa_shp %>% sf::st_buffer(dist = 1)
  africa_shp <- sf::as_Spatial(africa_shp)

  ##read in the folder to crop
  raster_list <- list.files(path = raster_folder,
                            pattern = ".tif")

  ### parallelization processing
  numCores <- min(numCores, parallel::detectCores())
  parallelMap::parallelLibrary("foreach")
  parallelMap::parallelLibrary("raster")
  parallelMap::parallelLibrary("sf")

  doParallel::registerDoParallel(cores = numCores) ##initiate the number of cores to be used

  foreach(i = 1:length(raster_list)) %dopar% {

    raster_tif <- raster::raster(paste(raster_folder, raster_list[i], sep = "/"))

    raster::crop(raster_tif, africa_shp, filename = paste0(raster_folder, "/NTL/", raster_list[i]))

  }

  endCluster()

}





