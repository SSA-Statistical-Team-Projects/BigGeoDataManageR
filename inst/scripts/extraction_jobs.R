### download CHIRPS data right from the database
download_dailychirps(start_date = "2018-11-01",
                     end_date = "2019-10-31",
                     exdir = "//esapov/esapov/ALL/Hydro/CHIRPS_Rainfall",
                     decomp_tfgiz = TRUE,
                     remove_original = TRUE)

### pull in more rainfall data
download_dailychirps(start_date = "2018-06-01",
                     end_date = "2018-10-31",
                     exdir = "//esapov/esapov/ALL/Hydro/CHIRPS_Rainfall",
                     decomp_tfgiz = TRUE,
                     remove_original = TRUE)


