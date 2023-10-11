#
# test_that("gengrid2 function assessment(squares)", {
#
#   grid_system <- gengrid2(shp_dt = gmbsf_dt,
#                           grid_size = 1000,
#                           sqr = TRUE)
#
#   # Perform a snapshot test for the output
#   expect_snapshot(grid_system)
#
#   # Expect no error
#   expect_no_error(grid_system)
#
# })
#
# test_that("gengrid2 function assessment(polygons)", {
#
#   grid_system_2 <- gengrid2(shp_dt = gmbsf_dt,
#                             grid_size = 1000,
#                             sqr = FALSE)
#
#   # Perform a snapshot test for the output
#   expect_snapshot(grid_system_2)
#
#   # Expect no error
#   expect_no_error(grid_system_2)
#
# })
