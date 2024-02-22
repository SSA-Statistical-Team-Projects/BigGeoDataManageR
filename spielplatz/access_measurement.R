##### a little script to test access measurement functions
pacman::p_load(osmdata, sfnetworks, accessibility, tidygraph, dbscan)

gmb_bbox <-
create_query_bbox(shp_dt = gmbsf_dt[gmbsf_dt$LGA == "Basse",],
                  buffer_dist = c(10000, 10000, 10000, 10000),
                  metric_crs = FALSE)

### creating centroids based on tesselation for distance matrix starting points

locations_dt <- gengrid2(shp_dt = gmbsf_dt[gmbsf_dt$LGA == "Basse",],
                         grid_size = 1000)

locations_dt <- st_centroid(x = locations_dt)

#### get the marketplace amenities

amenities_dt <-
  gmb_bbox %>%
  opq(timeout = 120, out = "body") %>%
  add_osm_feature("amenity", "marketplace") %>%
  osmdata_sf()

roadnetwork_obj <-
  gmb_bbox %>%
  opq(timeout = 200, out = "body") %>%
  add_osm_feature("highway", c("motorway", "primary", "secondary", "tertiary",
                               "unclassified", "residential", "trunk","road",
                               "motorway_link","trunk_link", "primary_link",
                               "secondary_link", "tertiary_link")) %>%
  osmdata_sf()


cleanlines_dt <-
  clean_osmlines(streets_obj = roadnetwork_obj)


locations_dt[["time_to_market"]] <-
  compute_networkaccess(lines_obj = cleanlines_dt,
                        origins_dt = locations_dt[,c("poly_id")],
                        dest_dt = amenities_dt$osm_points)





