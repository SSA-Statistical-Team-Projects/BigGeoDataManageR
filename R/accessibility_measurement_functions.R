################################################################################
############### A SET OF UTILITY FUNCTIONS TO SUPPORT THE PROJECT ##############
################################################################################

#' A function to parallel compute zonal stats
#'
#' @details This function computes the zonal statistics for a shapefile, preferably a large
#' shapefile with over 200,000+ observations by extracting vector object of class
#' `raster`. The function splits the `sf`, `data.frame` polygon object into `cpus` number
#' of parts and each part is extracted into in parallel.
#'
#' @param shp_dt sf, data.frame polygon/multipolygon object
#' @param raster_obj object of class raster to be extracted from
#' @param summary_fun the function to be used in computing zonal statistics
#' @param cpus number of CPUs for parallelization
#' @param parallel_mode see `parallelMap::parallelLapply()` for more details
#'
#' @export
#' @import parallelMap

parallel_zonalext <- function(shp_dt,
                              raster_obj,
                              summary_fun,
                              cpus,
                              parallel_mode = "multicore"){


  cpus <- min(cpus, parallel::detectCores())

  parallelMap::parallelStart(mode = parallel_mode,
                             cpus = cpus,
                             show.info = FALSE)

  if (parallel_mode == "socket") {
    parallel::clusterSetRNGStream()
  }

  ##### split shp_dt into equal parts
  shp_dt <- split(shp_dt, 1:cpus)

  compute_zonal_stats <- function(shp_dt,
                                  raster_obj,
                                  summary_fun){

    shp_dt$result <- exact_extract(x = raster_obj,
                                   y = shp_dt,
                                   fun = summary_fun)

    return(shp_dt)

  }

  parallelMap::parallelLibrary("exactextractr")

  result_dt <- parallelMap::parallelLapply(xs = shp_dt,
                                           fun = compute_zonal_stats,
                                           raster_obj = raster_obj,
                                           summary_fun = summary_fun)

  parallelMap::parallelStop()

  return(result_dt)

}

###############################################################################################

#' A function to create an osmdata package readable bounding box (bbox)
#' with a buffer distance
#'
#'
#' @export
#' @importFrom osmdata getbb
#' @importFrom sf st_bbox
#' @importFrom crsuggest suggest_crs
#' @importFrom raster extent
#' @importFrom dbscan dbscan
#' @import dplyr

create_query_bbox <- function(shp_dt = NULL,
                              area_name,
                              buffer_dist = c(0, 0, 0, 0),
                              metric_crs = FALSE,
                              osm_crs = 4326){

  if (is.null(shp_dt)){

    bbox_obj <- getbb(area_name)

    bbox_obj <- sf::st_bbox(raster::extent(bbox_obj),
                            crs = osm_crs)

    if (is.null(buffer_dist) == FALSE){

      ### convert to metric scale
      bbox_obj <- sf::st_as_sfc(x = bbox_obj,
                                crs = osm_crs)

      suggest_dt <- crsuggest::suggest_crs(bbox_obj, units = "m")

      bbox_obj <- st_transform(bbox_obj,
                               crs = as.numeric(suggest_dt$crs_code[1]))

      bbox_obj <- st_bbox(bbox_obj)

    }

  } else {

    if (metric_crs == FALSE) {

      suggest_dt <- crsuggest::suggest_crs(st_as_sfc(st_bbox(shp_dt)),
                                           units = "m")

      bbox_obj <- st_transform(st_as_sfc(st_bbox(shp_dt)),
                               crs = as.numeric(suggest_dt$crs_code[1]))
    } else {

      bbox_obj <- st_as_sfc(st_bbox(shp_dt))

    }

    bbox_obj <- sf::st_bbox(bbox_obj)

  }

  #### add buffer dist
  if (is.null(buffer_dist) == FALSE){

    bbox_obj[1] <- bbox_obj[1] - buffer_dist[1]
    bbox_obj[2] <- bbox_obj[2] - buffer_dist[2]
    bbox_obj[3] <- bbox_obj[3] + buffer_dist[3]
    bbox_obj[4] <- bbox_obj[4] + buffer_dist[4]


    ### recreate an st_as_sfc readable object
    if (metric_crs == TRUE) {

      bbox_obj <- sf::st_bbox(raster::extent(bbox_obj),
                              crs = st_crs(shp_dt))

    } else {

      bbox_obj <- sf::st_bbox(raster::extent(bbox_obj),
                              crs = as.numeric(suggest_dt$crs_code[1]))

    }

    bbox_obj <- st_as_sfc(bbox_obj)

    bbox_obj <- st_transform(bbox_obj,
                             crs = osm_crs)

    bbox_obj <- sf::st_bbox(bbox_obj)

    ## convert to osm_bbox type
    bbox_obj <- matrix(c(bbox_obj[[1]],
                         bbox_obj[[3]],
                         bbox_obj[[2]],
                         bbox_obj[[4]]),
                       ncol = 2,
                       byrow = TRUE)

    colnames(bbox_obj) <- c("min", "max")
    rownames(bbox_obj) <- c("x", "y")


  }

  return(bbox_obj)


}


#########################################################################################################################
#' A function to clean and prepare geospatial lines (road network) data for measuring cost matrices
#'
#' @param streets_obj an object of class `list`, `osmdata`, `osmdata_sf` for the path/road linestrings created from
#' `osmdata::add_osm_feature()` function
#' @param length_as_weight logical, if TRUE the geometry will be used in estimating the cost matrix for distance to destination
#' @param speed_dt a `data.frame`, `speed_dt` should contain two columns named `highway` and `speed` in km/hr according to the
#' OSM lines object from `osmdata` package. `speed` can be a column of 1s if the intention is not to estimate a cost beyond distances
#' @param surfadj_dt a `data.frame`, `surfadj_dt` should contain two columns `surface` and `div_speed_by` which allows to make
#' adjustments `speed_dt$speed` based on `surface` of each  `highway` class.
#' @param eps_dist a `numeric` to show the distance within which nodes that are close enough should be snapped together.
#' see `eps` argument of `dbscan::dbscan()`
#'
#'
#'
#'
#' @import tidygraph osmdata accessibility units crsuggest
#' @export

clean_osmlines <- function(streets_obj,
                           length_as_weight = TRUE,
                           speed_dt = data.frame(highway = c("trunk","secondary","tertiary", "residential",
                                                             "unclassified","primary","trunk_link", "secondary_link",
                                                             "primary_link","tertiary_link", "road" ),
                                                 speed = c(90, 60, 55, 40, 50, 70, 70, 55, 60, 45, 50)),
                           surfadj_dt = data.frame(surface = c("asphalt", "unpaved", "concrete","cobblestone:flattened",
                                                               "dirt/sand", "dirt", "paved", "sett", "ground",
                                                               "gravel" ,  "cobblestone", "paving_stones",
                                                               "compacted", "sand", "unhewn_cobblestone",
                                                               "earth", "grass" , "unspecified" , "mud" ,
                                                               "dust" ,  "wood", "concrete:plates",
                                                               "concrete:lanes", "unpaved;dirt/sand"),
                                                   div_speed_by = c(1.00, 1.30, 1.10, 1.30, 1.30, 1.10, 1.10, 1.30,
                                                                    1.30, 1.30, 1.30, 1.30, 1.00, 1.30, 1.30, 1.30,
                                                                    1.30, 1.00, 1.30, 1.30, 1.30, 1.00, 1.00, 1.30)),
                           eps_dist = 50,
                           directed = FALSE){

  lines_dt <- streets_obj$osm_lines[, c("osm_id", "highway", "surface", "geometry")]


  ### closed road is treated as a polygon so first convert to lines
  if (!is.null(streets_obj$osm_polygons)){

    closed_dt <- sf::st_cast(streets_obj$osm_polygons, "LINESTRING")

    closed_dt <- closed_dt[, c("osm_id", "highway", "surface", "geometry")]

    streets_obj$osm_lines <- rbind(lines_dt, closed_dt)

  }

  if (!is.null(streets_obj$osm_multipolygons)){

    closed_dt <- sf::st_cast(streets_obj$osm_multipolygons, "LINESTRING")

    closed_dt <- add_dt[, c("osm_id", "highway", "surface", "geometry")]

    streets_obj$osm_lines <- rbind(lines_dt, closed_dt)

  }

  if (!is.null(streets_obj$osm_multilines)){

    closed_dt <- sf::st_cast(streets_obj$osm_multilines, "LINESTRING")

    if (sum(colnames(closed_dt) %in% "surface") == 0){

      closed_dt$surface <- NA

    }

    closed_dt <- closed_dt[, c("osm_id", "highway", "surface", "geometry")]

    streets_obj$osm_lines <- rbind(lines_dt, closed_dt)

  }

  #### include the road speeds and speed adjustments
  lines_dt <-
    lines_dt %>%
    merge(speed_dt, all.x = TRUE) %>%
    merge(surfadj_dt, all.x = TRUE) %>%
    mutate(adj_speed = ifelse(is.na(surface),
                              speed,
                              speed / div_speed_by))

  ### creating an sf network object
  network_obj <- as_sfnetwork(lines_dt,
                              directed = directed,
                              length_as_weight = length_as_weight)

  ### simplifying the data, delete redundant edges, multiples edges and unnecessary
  ### loops

  smooth_action <-
    list(
      adj_speed = "mean",
      highway = function(x) if (length(unique(x)) == 1) x[1] else "unknown",
      "ignore"
    )

  network_obj <-
    network_obj %>%
    activate("edges") %>%
    arrange(edge_length()) %>%
    filter(!edge_is_multiple()) %>%
    filter(!edge_is_loop()) %>%
    convert(to_spatial_subdivision) %>%
    convert(to_spatial_smooth,
            summarise_attributes = smooth_action)

  ### using the dbscan algorithm to combine nodes that are so close
  #### use a metric resolution since eps is intuitive in metres

  node_dt <-
    network_obj %>%
    activate("nodes") %>%
    st_as_sf()

  suggest_dt <-
    node_dt %>%
    crsuggest::suggest_crs(units = "m")

  node_dt <-
    node_dt %>%
    st_transform(crs = as.numeric(suggest_dt$crs_code[1])) %>%
    st_coordinates()


  cluster_vector <- dbscan(node_dt,
                           eps = eps_dist,
                           minPts = 1)$cluster

  network_obj <-
    network_obj %>%
    activate("nodes") %>%
    mutate(cls = cluster_vector) %>%
    mutate(cmp = group_components()) %>%
    convert(to_spatial_contracted,
            cls,
            cmp,
            simplify = TRUE) %>%
    activate("edges") %>%
    st_as_sf() %>%
    mutate(speed = speed * units::as_units("km/h")) %>%
    mutate(adj_speed = adj_speed * units::as_units("km/h")) %>%
    mutate(weight = units::set_units(st_length(.), "km")) %>%
    mutate(time = weight / adj_speed)


  return(network_obj)

}




#' A function to compute distance/cost to point of interest using cost-matrix system
#'
#' @param lines_obj an `sf`,`data.frame` or `sfnetwork` object
#' @param origins_dt an `sf` object showing the set of origin points
#' @param dest_dt on `sf` object showing the destimations or points of interest to optimize on
#' @param cost_matrix where or not the cost matrix should be returned
#' @param weight the variable within the `lines_obj` to be used as measure cost measure (e.g time)
#' @param directed FALSE if each edge is assumed to be two-way directed.
#' @param length_as_weight TRUE if `weight` is to be length of edges within the network
#'
#' @export
#'

compute_networkaccess <- function(lines_obj,
                                  origins_dt,
                                  dest_dt,
                                  cost_matrix = FALSE,
                                  weight = "time",
                                  directed = FALSE,
                                  length_as_weight = TRUE){


  ### convert sf object to network object
  sfnetwork_obj <-
    lines_obj %>%
    as_sfnetwork(directed = directed,
                 length_as_weight = length_as_weight)

  network_crs <- sfnetwork_obj %>%
    st_as_sf() %>%
    st_crs()

  network_crs <- network_crs$input

  origins_crs <- origins_dt %>%
    st_crs()
  origins_crs <- origins_crs$input

  dest_crs <- dest_dt %>%
    st_crs()
  dest_crs <- dest_crs$input

  if (network_crs != origins_crs){

    origins_dt <- st_transform(origins_dt, crs = network_crs)

  }

  if (network_crs != dest_crs){

    dest_dt <- st_transform(dest_dt, crs = network_crs)

  }
  ### blend origin and destination data to the network object
  blend_obj <- st_network_blend(sfnetwork_obj, origins_dt)

  blend_obj <- st_network_blend(sfnetwork_obj, dest_dt)

  ### compute the cost matrix
  cost_matrix <- st_network_cost(blend_obj,
                                 from = origins_dt,
                                 to = dest_dt,
                                 weights = weight)


  min_values <- apply(cost_matrix, 1, min)

  return(min_values)

  if (cost_matrix == TRUE){

    return(list(min_values,
                cost_matrix))

  }


}


#' A function to compute distance/cost to point of interest using cost-matrix system in parallel
#'
#' @param cpus an `integer`, the number of CPUs to divide process into for parallelization
#' @param parallel_model see `parallelMap::parallelLapply()` for more details
#' @param lines_obj an `sfnetwork` object. apply as_`sfnetwork()` function to `sf`, `data.frame` object
#' @param origins_dt an `sf` object showing the set of origin points
#' @param dest_dt on `sf` object showing the destimations or points of interest to optimize on
#' @param cost_matrix where or not the cost matrix should be returned
#' @param weight the variable within the `lines_obj` to be used as measure cost measure (e.g time)
#' @param directed FALSE if each edge is assumed to be two-way directed.
#' @param length_as_weight TRUE if `weight` is to be length of edges within the network
#' @param blend_obj a `character`, for the filepath to .RDS file to a blended network object of network, origin and destination
#' objects. See `sfnetwork::st_network_blend()` for more details of `blend_obj`.
#' @param blend_dsn a `character`, the folder location to store `blend_obj` when the function creates it. Otherwise,
#' leave as NULL.
#'
#'
#' @export
#'


parallel_compnetaccess <- function(cpus,
                                   parallel_mode,
                                   lines_obj,
                                   origins_dt,
                                   dest_dt,
                                   cost_matrix = FALSE,
                                   weight = "time",
                                   directed = FALSE,
                                   length_as_weight = TRUE,
                                   blend_obj = NULL,
                                   blend_dsn = NULL){

  ### convert sf object to network object
  # sfnetwork_obj <-
  #   lines_obj %>%
  #   as_sfnetwork(directed = directed,
  #                length_as_weight = length_as_weight)

  sfnetwork_obj <- lines_obj

  network_crs <- sfnetwork_obj %>%
    st_as_sf() %>%
    st_crs()

  network_crs <- network_crs$input

  origins_crs <- origins_dt %>%
    st_crs()
  origins_crs <- origins_crs$input

  dest_crs <- dest_dt %>%
    st_crs()
  dest_crs <- dest_crs$input

  if (network_crs != origins_crs){

    origins_dt <- st_transform(origins_dt, crs = network_crs)

  }

  if (network_crs != dest_crs){

    dest_dt <- st_transform(dest_dt, crs = network_crs)

  }

  ### blend origin and destination data to the network object
  if (is.null(blend_obj)) {

    blend_obj <- st_network_blend(sfnetwork_obj, origins_dt)

    blend_obj <- st_network_blend(sfnetwork_obj, dest_dt)

    if (is.null(blend_dsn) == FALSE){

      saveRDS(blend_obj, paste0(blend_dsn, "/network_blend.RDS"))

    }

  }

  ### compute the cost matrix

  min_network_cost <- function(...){

    cost_matrix <- st_network_cost(...)

    min_values <- apply(cost_matrix, 1, min)

    return(min_values)

  }


  start_time <- Sys.time()

  if (cpus > 1) {
    cpus <- min(cpus, parallel::detectCores())

    origins_dt <-
      origins_dt %>%
      mutate(split_id = 1:nrow(.))

    dt_list <- split(origins_dt, 1:cpus)

    parallelMap::parallelStart(
      mode = parallel_mode,
      cpus = cpus,
      show.info = FALSE
    )

    if (parallel_mode == "socket") {
      parallel::clusterSetRNGStream()
    }

    parallelMap::parallelLibrary("sf")
    parallelMap::parallelLibrary("sfnetworks")

    min_values <- simplify2array(parallelMap::parallelLapply(
      xs                       = dt_list,
      fun                      = min_network_cost,
      x                        = blend_obj,
      to                       = dest_dt,
      weights                  = weight
    ))
    parallelMap::parallelStop()

  } else {

    "Please select a number greater than 1 for the cpus argument"

  }

  dt <-
    mapply(FUN = function(dt, cost){

      dt <- cbind(dt, cost)

      return(dt)

    },
    dt = dt_list,
    cost = min_values,
    SIMPLIFY = FALSE)

  dt <- Reduce(f = rbind,
               x = dt)


  return(dt)


}



