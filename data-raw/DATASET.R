## code to prepare datasets for the package

gmbsf_dt <- sf::read_sf(dsn = "//cwapov/cwapov/GMB/GEO/Boundaries",
                        layer = "LGA_Boundary")

usethis::use_data(gmbsf_dt, overwrite = TRUE)
