

### grid the data

grid_dt <- gengrid2(shp_dt = gmbsf_dt,
                    grid_size = 1000,
                    sqr = FALSE)

### apply the parallel zonal statistics computation
gmb_raster <- raster::raster("data/gmb_ppp_2020_constrained.tif")

start_time <- Sys.time()

parallel_zonalstats(x = gmb_raster,
                    y = grid_dt,
                    fun = "sum",
                    numCores = 5)


end_time <- Sys.time()

end_time - start_time


start_time <- Sys.time()

exact_extract(x = gmb_raster,
              y = grid_dt,
              fun = "sum")

end_time <- Sys.time()

end_time - start_time




