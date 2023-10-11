# test_that("get_chirps function assessment", {
#
#   # Define the appropriate test parameters
#   six_hour_intervals <- get_sixhr_chirps(start_date <- as.Date("2000-01-01"),
#                                          end_date <- as.Date("2000-01-01"),
#                                          start_hr <- 0,
#                                          end_hr <- 12,
#                                          link_base <- "https://data.chc.ucsb.edu/products/CHIRPS-2.0/",
#                                          dsn <- "~/Documents/GitHub/BigGeoDataManageR/data/rainfall_data",
#                                          cores <- 1L)
#
#   expect_snapshot(six_hour_intervals)
#   ## I need to test for the paths existed from manually downloading the data and the snapshot files created
# })
