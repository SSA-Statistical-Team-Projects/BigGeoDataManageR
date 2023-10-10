#' Another function to create a gridified shapefile and extract a raster if specified
#'
#' This function takes in only a shapefile and creates a square or hexagon polygon grid based on a specified
#' grid size
#'
#' @param shp_dt an object of class 'sf' or 'sfc'
#' @param shp_dsn character; the local directory folder in which the shapefile is location. Must be specified
#' when shp_dt is not specified.
#' @param shp_layer character; the layer name for the shapefile. Must be specified with shp_dsn when shp_dt is not
#' specified
#' @param grid_size numeric of length 1; representing the desired size of the grid in meters
#' @param sqr logical; if TRUE, a square grid is created. If FALSE, a hexagonal polygon grid is created
#' @param pop_raster raster; an object of class 'raster'
#' @param raster_path character; if pop_raster is not specified but raster is to read in from file. raster_path is
#' the full name of the raster (including filepath)
#' @param extract_name character of length 1; the name of the indicator to be extracted from the raster
#' @param raster_function function to be applied in extracting raster into created grids
#'
#' @importFrom raster raster
#' @importFrom raster cellStats
#' @importFrom units set_units
#'
#' @export


gengrid2 <- function(shp_dt,
                     grid_size,
                     sqr = TRUE) {

  sf_use_s2(FALSE) ##just to ensure we don't begin to have issues with duplicate vertices

  ## now we are ready to grid our district shapefile
  print("Initiating shape object tesselation")
  if (sqr == TRUE) {

    grid_system <- st_make_grid(x = shp_dt,
                                cellsize = c(grid_size, grid_size),
                                square = sqr) %>%
      sf::st_sf()

  } else if (sqr == FALSE) {

    grid_system <- st_make_grid(x = shp_dt,
                                cellsize = grid_size,
                                square = sqr) %>%
      sf::st_sf()


  }
  print("Tesselation complete for shapefile extent, ensuring validity of shapefile ...")
  ## the process creates squares with parts outside the area so we should take the intersection
  ## of the shapefile with our newly created grid

  ## to avoid failures we need to make sure geometries are valid
  shp_checklist <- st_is_valid(shp_dt)

  while(sum(shp_checklist) != length(shp_checklist)){

    shp_dt <- st_make_valid(shp_dt)
    shp_checklist <- st_is_valid(shp_dt)

  }
  print("Limiting tesselated object to shapefile area ...")

  ## figure out which grids belong within the shapefile
  grid_system$poly_id <- 1:nrow(grid_system)
  grid_system <- st_join(grid_system, shp_dt, left = F, largest = TRUE)

  ## compute area of duplicated grids and assign shp_dt areas

  print("The shapefile is fully gridded!!")

  ## make sure all geometries are polygons
  clean_geometry <- function(geo_dt){

    geo_dt <- geo_dt[st_dimension(geo_dt) == 2,] ##ensure that all geometries are surfaces
    ##find any other enclosure geometries
    add_dt <- geo_dt[st_geometry_type(geo_dt) == "GEOMETRYCOLLECTION",]
    add_dt <- st_collection_extract(add_dt)
    add_dt <- add_dt[st_dimension(add_dt) == 2,]
    geo_dt <- geo_dt[!(st_geometry_type(geo_dt) == "GEOMETRYCOLLECTION"),]

    geo_dt <- rbind(geo_dt, add_dt)


    return(geo_dt)

  }

  grid_system <- clean_geometry(grid_system)

  print("Ensuring geometries are properly fixed")
  grid_system$poly_id <- 1:nrow(grid_system)

  grid_system$poly_area <- st_area(grid_system) ##compute area of each square
  grid_system$poly_area <- set_units(grid_system$poly_area, "km^2")

  grid_system <- grid_system[as.numeric(grid_system$poly_area) > 0,]

  print(paste0("The tesselated object represents a total area of ",
               round(sum(grid_system$poly_area, na.rm = TRUE),2),
               " km^2"))

  grid_check <- as.numeric(grid_system$poly_area)
  hist(x = grid_check,
       xlab = "Polygon Size (in km^2)",
       main = "Distribution of Polygon Size")

  print("The plot window should show you a distribution of the polygon sizes")

  return(grid_system)

}






#' Automate the process of pulling building data from the World Pop website
#'
#' @param iso The standard ISO-3 country code for country of interest
#' @param wpversion The World Pop version of interest (use wpopbuilding_vcheck function to check version availability)
#' @param ldrive_dsn Destination for the downloaded files
#'
#'
#' @details For more information on documentation please run wpopbuilding_pull(iso = "XXX", wpversion, ldrive_dsn). This should
#' download the documentation complete with data file descriptions and citation references
#'
#' @export
#'
#' @import data.table
#' @importFrom RCurl getURL

wpopbuilding_pull <- function(iso = "BEN",
                              wpversion = "v2.0",
                              ldrive_dsn = "data-raw"){


  #construct the link to the world pop website with the building links
  url <- paste("ftp://ftp.worldpop.org/repo/wopr/_MULT/buildings", wpversion, "", sep = "/")

  #list the file names present on the specific version sub-website for world pop building
  filenames <- getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  filenames <- strsplit(filenames, "\r\n")
  filenames <- unlist(filenames)

  filenames <- as.data.table(filenames)
  filenames[,country := substr(filenames, 1, 3)]

  #return a message if country not on the list was selected, otherwise go ahead and download data and store locally
  if((iso %in% filenames[,country]) == FALSE){

    return("this country is not within this database")

  } else {

    rel_fn <- filenames[country == iso,filenames]
    url <- paste(url, rel_fn, sep = "")

    return(download.file(url = url, destfile = paste(ldrive_dsn, rel_fn, sep = "/")))

  }


}


#' This function returns a datatable of all available versions of World Pop raster
#' building data in each country with available
#'
#' simply run : wpopbuilding_check() in your console.
#'
#' @note ALWAYS GIVE AT LEAST 2 MINUTES BETWEEN SUCCESSIVE RUNS OF THIS FUNCTION. THE PREVENTS SUBSEQUENT REQUESTS ON ITS
#' SERVER WITHIN THAT TIME INTERVAL, SO YOU WILL GET A <NOT SET> FUNCTION ERROR.
#'
#' @return a data.table object with list and versions of building data
#'
#' @import data.table
#' @importFrom RCurl getURL
#' @export

wpopbuilding_vcheck <- function(){

  building_link <- "https://data.worldpop.org/repo/wopr/_MULT/buildings/"

  #construct the link to the world pop website with the building links
  #list the file names present on the specific version sub-website for world pop building
  find_sublinks <- function(url = building_link){

    versions <- RCurl::getURL(url = building_link,
                              ftp.use.epsv = FALSE,
                              dirlistonly = TRUE)
    versions <- strsplit(versions, "\r\n")
    versions <- unlist(versions)

    return(versions)

  }

  bld_vers <- find_sublinks() #function execution

  find_blddt <- function(versions){

    ##construct link
    suburl <- paste(building_link, versions, "", sep = "/")

    filenames <- find_sublinks(url = suburl)
    filenames <- data.table::as.data.table(filenames)
    filenames[,country := substr(filenames, 1, 3)]
    filenames[,versionnum := versions]

    return(filenames)
  }



  all_filenames <- data.table::rbindlist(lapply(bld_vers, find_blddt))

  return(all_filenames)

}




#### ---------------------------------------------------------------------- ####

#' Compute zonal statistics in-parallel
#'
#' A function to split a shapefile into many parts and computes the zonal statistics
#' for each part in parallel to speed up the zonal statistics estimations
#'
#' @param x a `RasterLayer`, `RasterStack`, `RasterBrick`, or `SpatRaster` with data
#' to be extracted from
#' @param y a `sf`, `sfc`, `SpatialPolygonsDataFrame`, or `SpatialPolygons` object
#' with polygonal geometries to be extracted into
#' @param fun the function to be used compute zonal statistics
#' @param numCores the number of cores to be used in parallel processing
#'
#'
#' @examples
#'
#' \donttest {
#'
#' grid the data
#'
#' grid_dt <- gengrid2(shp_dt = gmbsf_dt,
#'                     grid_size = 1000,
#'                     sqr = FALSE)
#'
#' ### apply the parallel zonal statistics computation
#' gmb_raster <- raster::raster("data/gmb_ppp_2020_constrained.tif")
#'
#' start_time <- Sys.time()
#'
#' grid_dt$population <-
#' parallel_zonalstats(x = gmb_raster,
#'                     y = grid_dt,
#'                     fun = "sum",
#'                     numCores = 5)
#'
#'
#'
#' }
#'
#'
#' @export



parallel_zonalstats <- function(x,
                                y,
                                fun,
                                numCores){

  ### parallelization processing
  numCores <- min(numCores, parallel::detectCores())
  parallelMap::parallelLibrary("foreach")
  parallelMap::parallelLibrary("raster")
  parallelMap::parallelLibrary("sf")
  parallelMap::parallelLibrary("exactextractr")

  doParallel::registerDoParallel(cores = numCores) ##initiate the number of cores to be used

  numparts <- ceiling(nrow(y) / numCores)

  y$part <- rep(1:numCores,
                each = numparts,
                length.out = nrow(y))

  y <- split(y, y$part)

  results_dt <-
  foreach(i = 1:numCores) %dopar% {

    exactextractr::exact_extract(x = x,
                                 y = y[[i]],
                                 fun = fun)


  }

  endCluster()

  results_dt <- unlist(results_dt)


  return(results_dt)

}





























