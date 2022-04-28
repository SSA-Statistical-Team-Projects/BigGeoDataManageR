#' A function for cropping many large rasters in parallel to a specific area
#'
#' The function will crop large rasters in a specific location to the bounding box of a specific shapefile given a buffer area
#'
#' @param raster_folder the filepath folder on local drive storing the set of rasters to be cropped
#' @param shp_dsn see dsn in sf::st_read()
#' @param shp_layer see layer in sf::st_read()
#' @param numCores the number of cores to extract each raster in parallel
#' @param out_folder the output folder for cropped rasters
#'
#' @import parallelMap raster sf foreach
#'
#' @export







bgdm_parallelcrop <- function(raster_folder = "//esapov/esapov/ALL/Energy",
                              shp_dsn,
                              shp_layer,
                              numCores = 15,
                              out_folder){

  sf::sf_use_s2(FALSE)
  ##read in africa shapefile
  shp_dt <- sf::st_read(dsn = shp_dsn,
                            layer = shp_layer)

  shp_dt <- shp_dt %>% sf::st_bbox() %>% sf::st_as_sfc()
  shp_dt <- shp_dt %>% sf::st_buffer(dist = 1)
  shp_dt <- sf::as_Spatial(shp_dt)

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

    raster::crop(raster_tif, shp_dt, filename = paste0(out_folder, raster_list[i]))

  }

  endCluster()

}

