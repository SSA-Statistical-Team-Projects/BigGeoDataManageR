# test_that("get_ntl function assessment", {
#   # Define the appropriate test parameters
#   daily_nighttime <- get_daily_ntl("iedochie@worldbank.org",
#                                       "I651ny9@1588",
#                                       as.Date("2015-04-18"),
#                                       as.Date("2015-04-20"),
#                                       link_base = "https://eogdata.mines.edu/nighttime_light/nightly",
#                                       'npp',
#                                       cloud_mask = T,
#                                       cores = 1L)
#
#   annual_nighttime <- get_annual_ntl(username = "iedochie@worldbank.org",
#                                      password = "I651ny9@1588",
#                                      year = 2015,
#                                      version = "v20",
#                                      indicator = "average_masked")
#
#   # Before testing, exist status for the daily nighttime data example generated seems to not exist. What
#   # do you recommend?
#
#
#   ## Test for the snapshot of this data
#   expect_snapshot(six_hour_intervals)
# })
