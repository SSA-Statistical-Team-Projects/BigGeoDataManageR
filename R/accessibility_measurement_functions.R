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
#' @param speed_dt a `data.frame`, `speed_dt` should contain two columns named `highway` and `speed` according to the OSM lines
#' object from `osmdata` package. `speed` can be a column of 1s if the intention is not to estimate a cost beyond distances
#' @param surfadj_dt a `data.frame`, `surfadj_dt` should contain two columns surface and div_speed_by which allows to make
#' adjustments `speed_dt$speed` based on `surface` of each  `highway` class.
#' @param eps_dist a `numeric` to show the distance within which nodes that are close enough should be merged. see `eps` argument
#' of `dbscan::dbscan()`
#'
#'
#'
#'
#' @import tidygraph osmdata accessibility

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
                           eps_dist = 50){

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
                              directed = F,
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
  node_dt <-
    network_obj %>%
    activate("nodes") %>%
    st_coordinates()

  cluster_vector <- dbscan(node_dt,
                           eps = eps_dist/(6371000*180/22/7),
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
    mutate(weight = units::set_units(st_length(.), "km")) %>%
    mutate(time = weight / adj_speed)





  return(network_obj)



}




#' A function to compute distance/cost to point of interest using cost-matrix system
#'

compute_networkaccess <- function(sfnetwork_obj,
                                  origins_dt,
                                  dest_dt,
                                  cost_matrix = FALSE,
                                  weight = "time"){


  ### blend origin and destination data to the network object
  blend_obj <- st_network_blend(sfnetwork_obj, origins_dt)

  blend_obj <- st_network_blend(sfnetwork_obj, dest_dt)

  ### compute the cost matrix
  cost_matrix <- st_network_cost(blend_obj,
                                 from = origins_dt,
                                 to = dest_dt,
                                 weight = weight)


  min_values <- apply(cost_matrix, 1, min)

  return(min_values)


}






