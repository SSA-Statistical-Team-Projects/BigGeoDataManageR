#' Download Night Time Lights Data
#'
#' Functions to download night time lights data from NASA's Earth Observation
#' Group's (EOG) database. The downloaded rasters are collected for different
#' time intervals, daily, monthly and annually.
#'
#' @param start_date An object of class date, must be specified like as.Date("yyyy-mm-dd")
#' @param end_date An object of class date, must be specified like as.Date("yyyy-mm-dd")
#' @param link_base the link to the daily chirps data (no need to change this)
#'
#' @importFrom parallel stopCluster makePSOCKcluster parLapply
#'
#' @export
#'


get_daily_ntl <- function(start_date,
                          end_date,
                          link_base = "https://eogdata.mines.edu/nighttime_light/nightly/",
                          satel_name,
                          cloud_mask = T,
                          client_id = 'eogdata_oidc',
                          client_secret = '2677ad81-521b-4869-8480-6d05b9e57d48',
                          username = "iedochie@worldbank.org",
                          password = "I651ny9@1588",
                          grant_type = 'password'){


  ### first create the filename list
  name_list <- name_daily_ntl(start_date = start_date,
                              end_date = end_date,
                              satel_name = satel_name,
                              cloud_mask = cloud_mask)


  ### create the full link set
  name_dt <- data.table(filename = name_list,
                        satellite = satel_name,
                        cloud_mask = ifelse(cloud_mask == TRUE, "cloud_cover", "rade9d"))

  name_dt[, folder := ifelse(satellite == "j01", "_j01", "")]
  name_dt[, folder := paste0(satellite, folder)]


  name_dt[, full_link := paste(link_base, folder, filename, sep = "/")]


  ### call function to perform download




}

