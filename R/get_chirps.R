#' Download six-hour rainfall chirp data
#'
#' @param start_hr An integer, the starting hour in the interval for pulling the data
#' @param end_hr An integer, the final hour in the interval for pulling the data
#' @param start_date An object of class date, must be specified like as.Date("yyyy-mm-dd")
#' @param end_date An object of class date, must be specified like as.Date("yyyy-mm-dd")
#' @param link_base the link to the daily chirps data (no need to change this)
#' @param dir_name subfolder within `link_base` of interest
#'

get_sixhr_chirps <- function(start_date,
                             start_end,
                             start_hr,
                             end_hr,
                             link_base = "https://data.chc.ucsb.edu/products/CHIRPS-2.0/"){

  ## construct link to the file locations
  dt <- chirpname_sixhr(start_date = start_date,
                        end_date = end_date,
                        start_hr = start_hr,
                        end_hr = end_hr)


  dt[, dir_name := "africa_6-hourly/p1_bin"]

  dt[, full_link := paste0(link_base,
                           dir_name,
                           paste0(year, month))]


}
