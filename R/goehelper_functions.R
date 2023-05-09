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


#' This function does simple cleaning of the OSM data for lines, multipolygons and points
#'
#'
#' @param dropNA_cols if TRUE, unnamed classifications of features are dropped. Note: This is different from the
#' label "unclassified" as some features are tagged "NA" in OSM data (default is FALSE)
#' @param zero_NA if TRUE, NA observations are left as zero (default is FALSE)
#' @param missing_threshold numeric, if zero_NA is TRUE, missing_threshold is a value between 0 and 1. Columns with a rate
#' of missing observations less than missing_threshold will have missing values converted to 0.
#'
#' @export
#'
#' @return a cleaned data.table object




osm_cleanprocess <- function(dt,
                             dropNA_cols = TRUE,
                             zero_NA = TRUE,
                             missing_threshold = 0.3){

  ## if condition is met, we drop calls that contain NA in the column names
  if (dropNA_cols == TRUE){

    na_cols <- colnames(dt)[grepl("NA", colnames(dt))]
    dt[,(na_cols) := NULL]

  }

  ## if condition is met, we replace missing observations in variables below threshold rate of missing with zeros

  if (zero_NA == TRUE){

    ## figure out what variables are under the missingness threshold
    compute_missingrate <- function(X){

      value <- ifelse(is.na(X), 1, 0)
      value <- mean(value, na.rm = TRUE)
      return(value)

    }

    missings <- dt[,lapply(.SD, compute_missingrate)]
    ## restructure this information into a data.table with missing rate and corresponding variable name
    names_miss <- names(missings)
    missings <- as.data.table(unlist(missings))
    missings[,names_miss := names_miss]

    missings <- missings[(V1 <= missing_threshold),]


    replace_NA <- function(X){

      X[is.na(X)] <- 0

      return(X)
    }

    dt <- dt[, lapply(.SD, replace_NA), .SDcols = missings$names_miss]
  }

  return(dt)
}





#' This function pulls specific layer data (lines, multipolygons and points) from the Open Street Maps for countries or regions
#'
#' @param country a string argument for the country name (or city) of interest i.e. place argument for oe_get function
#' @param ldrive a string argument for the local folder/drive location where the data should be stored
#' @param lines if TRUE, OSM features with lines data will be pulled
#' @param points if TRUE, OSM features with points data will be pulled
#' @param multipolygon if TRUE, OSM features will be pulled within polygons
#'
#' @return one shapefiles for each of lines, points and/or multipolygon pulls specified as TRUE
#'
#' @importFrom osmextract oe_get
#'
#'
#' @export


osm_datapull <- function(country = "Cameroon",
                         ldrive = "C:/Users/ifean/Documents/WorldBankWork/SAEPlus_Other",
                         lines = T,
                         points = T,
                         multipolygon = T){

  if (lines == TRUE){

    osm_lines <- oe_get(country, stringsAsFactors = TRUE)
    file <- paste(country, "osmlines", sep = "_")
    save (osm_lines, file = paste(ldrive, file, sep = "/"))

  }

  if(points == TRUE){

    osm_points <- oe_get(country, layer="points", stringsAsFactors = FALSE)
    file <- paste(country, "osmpoints", sep = "_")
    save (osm_points, file = paste(ldrive, file, sep = "/"))

  }

  if(multipolygon == TRUE) {

    osm_mp <- oe_get(country, layer="multipolygons", stringsAsFactors = TRUE)
    file <- paste(country, "osmmp", sep = "_")
    save (osm_mp, file = paste(ldrive, file, sep = "/"))

  } else {"You have not specified any of lines, points and multipolygon arguments as TRUE"}

}



#' This function cleans up OSM lines data in preparation for survey to survey imputation analysis
#'
#' @param shapefile_path filepath of country/region shapefile with polygons of interest
#' @param osm_path filepath to open street map lines data (road networks)
#' @param geoid_var the variable that points to the common identifier ID between shapefile_path object and osm_path object
#' @param feature_var specific feature of interest in the osm_path object
#'
#' @return A list containing two objects: a dataframe/datatable with original osm points data containing number of points
#' in polygon shapefile (from shapefile_path argument) and a lazy datatable version of the full osm points data
#' with point information
#'
#' @export
#'
#' @import data.table sf dtplyr


osm_processpoints <- function(shapefile_path,
                              osm_path,
                              geoid_var,
                              feature_var){

  agebs <- sf::st_read(shapefile_path)
  load(osm_path)
  osm_agebs <- dtplyr::lazy_dt(st_intersection(osm_points,st_make_valid(agebs)))

  out.dt <- osm_agebs$parent[,count := .N,by = c(geoid_var, feature_var)]

  agebs.dt <- data.table::as.data.table(agebs)
  joined.dt <- out.dt[agebs.dt, on = geoid_var]

  ## flip the data from long to wide
  arg_form <- paste(paste(geoid_var, collapse = " + "), "~",
                    paste(feature_var, collapse = " + "))

  wide_join.dt <- dcast(joined.dt, formula = formula(arg_form),
                        value.var = "count",
                        fun.aggregate = mean)

  wide_join.dt <- agebs.dt[wide_join.dt, on = geoid_var]

  wide_join.dt <- osm_cleanprocess(dt = wide_join.dt)

  return(list(long_mp.dt = joined.dt,
              wide_mp.dt = wide_join.dt))

}


#' A simple function to include benchmarked results to preferred file
#'
#' This function is built to take the results of the benchmarked poverty rates and use them to replace
#' the original ebp estimates that were written to an xlsx file using the SAEplus::emdi_writeexcel()
#' or emdi::write.excel() functions
#'
#' @param ebp_obj an object of class "ebp"
#' @param csv_file full file name (including path) where results will be outputted
#' @param bm_obj an object of class data.table / data.frame which is the result of benchmarking the poverty rates
#' (usually the output of the function saeplus_calibratepovrate())
#' @param bm_unitid the id variable within the bm_obj object argument
#'
#' @import data.table
#'
#' @export
#'


saeplus_addbenchmark <- function(ebp_obj = ginemdi_model2,
                                 csv_file = "data/emdi_results2.csv"){

  ## combine the MSE and IND results from ebp object
  dt <- as.data.table(left_join(ginemdi_model2$MSE, ginemdi_model2$ind, by = "Domain"))

  setnames(dt, c("Mean.x", "Head_Count.x", "Mean.y", "Head_Count.y"),
           c("Mean_MSE", "Head_Count_MSE", "Mean", "Head_Count"))

  dt <- dt[,Head_Count := NULL]

  dt[,Mean_CV := sqrt(Mean_MSE) / Mean]
  dt[,Head_Count_CV := sqrt(Head_Count_MSE) / BM_Head_Count]


  ### write results to new sheet of xlsx file

  write.csv(dt, file = csv_file)

  return(dt)

}



#' Benchmark Small Area Estimates
#'
#' This function benchmarks the SAE model estimates to match survey estimates at level of chosen level of
#' representativeness.
#'
#' @param hh_dt a household data.frame/data.table object containing area level IDs for which poverty rates were small area
#' estimated
#' @param pop_dt a population level data.frame/data.table object containing geospatial polygons and areas level IDs used in hh_dt
#' for which population level estimates can be computed
#' @param ebp_obj an object of class "emdi" "ebp" obtained from small area estimation containing area level poverty rates.
#' Area Level IDs must be matching across pop_dt, hh_dt and ebp_obj
#' @param area_id the level at which the poverty areas were estimated
#' @param harea_id a higher level at which intermediate SAE estimates will be computed
#' @param povline national poverty line to be used with the hh_dt object to determine survey level poverty rates
#' @param weight household level weights within the hh_dt object
#' @param excl_outsample whether or not outsample grids should be excluded in the benchmarking exercise
#'
#' @import data.table
#'
#' @export


saeplus_calibratepovrate <- function(hh_dt,
                                     pop_dt,
                                     ebp_obj,
                                     area_id,
                                     pop_var,
                                     harea_id,
                                     povline,
                                     welfare,
                                     weight,
                                     excl_outsample = TRUE){

  hh_dt <- as.data.table(hh_dt)
  pop_dt <- as.data.table(pop_dt)

  pop_doms <- unique(pop_dt[,get(area_id)])
  survey_doms <- unique(hh_dt[,get(area_id)])

  if(excl_outsample == TRUE){

    excl_doms <- pop_doms[!(pop_doms %in% survey_doms)]

    pop_dt <- pop_dt[!(get(area_id) %in% excl_doms),]
  }
  #1. Generate population estimate for each SAE level ID by aggregating across grids
  pop_size <- pop_dt[,sum(get(pop_var), na.rm = TRUE),by = get(area_id)]
  # pop_size <- hh_dt[,sum(get(weight)), by = area_id]
  setnames(pop_size, colnames(pop_size), c(area_id, "population"))

  #### merge in h_area ID and poverty rates from EMDI object
  pop_size <- pop_dt[,c(area_id, harea_id),with=F][pop_size, on = area_id]
  pop_size <- unique(pop_size)
  povrate.dt <- as.data.table(ebp_obj$ind)
  povrate.dt[,Domain := as.integer(as.character(Domain))]
  setnames(povrate.dt, "Domain", area_id)
  pop_size <- povrate.dt[,c(area_id, "Head_Count"),with=F][pop_size, on = area_id]

  #2.	Use these population estimates for each sampled sub-prefecture as weights to generate small area estimates for
  #   each state. Call these the sae state estimates.
  #         a.	Take the sum of population*poverty rate / total population, by sub-prefecture
  #3. Estimate state level poverty rates
  pop_size[,harea_popsize := sum(population, na.rm = TRUE), by = harea_id]
  pop_size[,area_povrate_share := population * Head_Count / harea_popsize]
  pop_size[,harea_povrate := sum(area_povrate_share, na.rm = TRUE), by = harea_id]

  hh_dt[,povline := ifelse(get(welfare) < povline, 1, 0)]
  harea_povline <- hh_dt[,weighted.mean(povline, get(weight)), by = harea_id]
  setnames(harea_povline, "V1", "survey_povrate")
  harea_povline <- harea_povline[!is.na(harea_id),]

  #4.	Divide the vector of state survey estimates by sae estimates to get the benchmarking ratio.
  #5.	Multiply the point estimates by the benchmarking ratio.

  pop_size <- harea_povline[pop_size, on = harea_id]

  pop_size[, bmratio := survey_povrate / harea_povrate]

  pop_size[, BM_Head_Count := Head_Count * bmratio]

  return(pop_size)

}


saeplus_calibratepovrate2 <- function(hh_dt,
                                      pop_dt,
                                      ebp_obj,
                                      area_id,
                                      pop_var,
                                      harea_id,
                                      povline,
                                      welfare,
                                      weight,
                                      excl_outsample = TRUE){

  hh_dt <- as.data.table(hh_dt)
  pop_dt <- as.data.table(pop_dt)

  pop_doms <- unique(pop_dt[,get(area_id)])
  survey_doms <- unique(hh_dt[,get(area_id)])


  if(excl_outsample == TRUE){

    excl_doms <- pop_doms[!(pop_doms %in% survey_doms)]

    pop_dt <- pop_dt[!(get(area_id) %in% excl_doms),]
  }
  #1. Generate population estimate for each SAE level ID by aggregating across grids
  pop_size <- pop_dt[,sum(get(pop_var), na.rm = TRUE),by = get(area_id)]
  # pop_size <- hh_dt[,sum(get(weight)), by = area_id]
  setnames(pop_size, colnames(pop_size), c(area_id, "population"))

  #### merge in h_area ID and poverty rates from EMDI object
  pop_size <- pop_dt[,c(area_id, harea_id),with=F][pop_size, on = area_id]
  pop_size <- unique(pop_size)
  povrate.dt <- as.data.table(ebp_obj$ind)
  povrate.dt[,Domain := as.integer(as.character(Domain))]
  setnames(povrate.dt, "Domain", area_id)
  pop_size <- povrate.dt[,c(area_id, "Head_Count"),with=F][pop_size, on = area_id]

  #2.	Use these population estimates for each sampled sub-prefecture as weights to generate small area estimates for
  #   each state. Call these the sae state estimates.
  #         a.	Take the sum of population*poverty rate / total population, by sub-prefecture
  #3. Estimate state level poverty rates
  pop_size[,harea_popsize := sum(population, na.rm = TRUE), by = harea_id]
  pop_size[,npHead_Count := 1 - Head_Count]
  pop_size[,area_nprate_share := population * npHead_Count / harea_popsize]
  pop_size[,harea_nprate := sum(area_nprate_share, na.rm = TRUE), by = harea_id]

  hh_dt[,povline := ifelse(get(welfare) < povline, 1, 0)]
  harea_povline <- hh_dt[,weighted.mean(povline, get(weight)), by = harea_id]
  setnames(harea_povline, "V1", "survey_povrate")
  harea_povline[,survey_nprate := 1 - survey_povrate]
  harea_povline <- harea_povline[!is.na(harea_id),]


  #4.	Divide the vector of state survey estimates by sae estimates to get the benchmarking ratio.
  #5.	Multiply the point estimates by the benchmarking ratio.

  pop_size <- harea_povline[pop_size, on = harea_id]

  pop_size[, bmratio := survey_nprate / harea_nprate]

  pop_size[, npBM_Head_Count := npHead_Count * bmratio]

  pop_size[, BM_Head_Count := 1 - npBM_Head_Count]

  return(pop_size)

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
