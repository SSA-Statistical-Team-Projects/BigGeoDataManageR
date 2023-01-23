#' Download six-hour rainfall chirp data
#'
#' @param start_hr An integer, the starting hour in the interval for pulling the data
#' @param end_hr An integer, the final hour in the interval for pulling the data
#' @param start_date An object of class date, must be specified like as.Date("yyyy-mm-dd")
#' @param end_date An object of class date, must be specified like as.Date("yyyy-mm-dd")
#' @param link_base the link to the daily chirps data (no need to change this)
#' @param resolution A character, gridded resolution of the data (options area "p05" i.e.
#'                   0.05 degree resolution and p25 for 0.25)
#'
#' @importFrom parallel stopCluster makePSOCKcluster parLapply
#'

get_sixhr_chirps <- function(start_date,
                             end_date,
                             start_hr,
                             end_hr,
                             link_base = "https://data.chc.ucsb.edu/products/CHIRPS-2.0/",
                             dsn = getwd(),
                             cores = 1L){

  ## construct link to the file locations
  dt <- chirpname_sixhr(start_date = start_date,
                        end_date = end_date,
                        start_hr = start_hr,
                        end_hr = end_hr)


  dt[pull_date <= as.Date("2015-09-30"), sub_dir := paste0("africa_6-hourly/p1_bin/", year, month)]
  dt[pull_date > as.Date("2015-09-30"), sub_dir := paste0("africa_6-hourly/p1_bin/extra_step/", year, month)]

  dt[, full_link := paste0(link_base,
                           sub_dir,
                           "/",
                           filename)]

  cl <- makePSOCKcluster(cores)
  on.exit(stopCluster(cl))


  ## check that the link exists
  dt$exist_status <- unlist(parLapply(cl = cl,
                                      X = dt$full_link,
                                      fun = checkurl_exist))

  ## download the chirps
  parallel::parLapply(cl = cl,
                      X = dt[exist_status == TRUE, full_link],
                      fun = download_worker,
                      dsn = dsn)

}



get_daily_chirps <- function(start_date,
                             end_date,
                             link_base = "https://data.chc.ucsb.edu/products/CHIRPS-2.0",
                             dsn = getwd(),
                             cores = 1L,
                             resolution = "p05"){

  ## construct link to the file locations
  daily_list <- chirpname_daily(start_date = start_date,
                                end_date = end_date)


  ###construct full path
  construct_chirplink <- function(X){

    get_date <- str_extract(X, "[0-9]{4}.[0-9]{2}.[0-9]{2}")
    get_date <- gsub("\\.", "-", get_date)
    get_date <- as.Date(get_date)

    link_year <- year(get_date)

    url <- paste(link_base, "global_daily", "tifs", resolution, link_year, X, sep = "/")

    return(url)

  }

  cl <- makePSOCKcluster(cores)
  on.exit(stopCluster(cl))

  url_list <- unlist(lapply(daily_list,
                            construct_chirplink))

  dt <- data.table(full_link = url_list)

  ## check that the link exists
  dt$exist_status <- unlist(parLapply(cl = cl,
                                      X = dt$full_link,
                                      fun = checkurl_exist))

  ## download the chirps
  parallel::parLapply(cl = cl,
                      X = dt[exist_status == TRUE, full_link],
                      fun = download_worker,
                      dsn = dsn)


}



get_month_chirps <- function(start_date,
                             end_date,
                             link_base = "https://data.chc.ucsb.edu/products/CHIRPS-2.0/",
                             dsn = getwd(),
                             cores = 1L){

  dt <- chirpname_monthly(start_date = start_date,
                          end_date = end_date)

  dt[, sub_dir := "global_monthly/tifs/"]

  dt[, full_link := paste0(link_base,
                           sub_dir,
                           filename)]

  cl <- makePSOCKcluster(cores)
  on.exit(stopCluster(cl))

  ## check that the link exists
  dt$exist_status <- unlist(parLapply(cl = cl,
                                      X = dt$full_link,
                                      fun = checkurl_exist))

  ## download the chirps
  parallel::parLapply(cl = cl,
                      X = dt[exist_status == TRUE, full_link],
                      fun = download_worker,
                      dsn = dsn)


}



get_annual_chirps <- function(start_year,
                              end_year,
                              link_base = "https://data.chc.ucsb.edu/products/CHIRPS-2.0/",
                              dsn = getwd(),
                              cores = 1L){


  dt <- chirpname_annual(start_year = start_year,
                         end_year = end_year)

  dt <- data.table(filename = dt)

  dt[, sub_dir := "global_annual/tifs/"]

  dt[, full_link := paste0(link_base,
                           sub_dir,
                           filename)]

  cl <- makePSOCKcluster(cores)
  on.exit(stopCluster(cl))

  ## check that the link exists
  dt$exist_status <- unlist(parLapply(cl = cl,
                                      X = dt$full_link,
                                      fun = checkurl_exist))

  ## download the chirps
  parallel::parLapply(cl = cl,
                      X = dt[exist_status == TRUE, full_link],
                      fun = download_worker,
                      dsn = dsn)


}







