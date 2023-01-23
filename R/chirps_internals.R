#' Internal functions
#'
#' @param start_hr An integer, the starting hour in the interval for pulling the data
#' @param end_hr An integer, the final hour in the interval for pulling the data
#' @param start_date An object of class date, must be specified like as.Date("yyyy-mm-dd")
#' @param end_date An object of class date, must be specified like as.Date("yyyy-mm-dd")
#'
#' @importFrom lubridate is.Date ymd_hms ceiling_date floor_date day hour year month format_ISO8601
#' @importFrom data.table data.table


chirpname_sixhr <- function(start_date,
                            end_date,
                            start_hr,
                            end_hr,
                            repo_interval = "6 hours",
                            filename_tag = "rfe_gdas.bin.",
                            file_ext = ".gz"){


 #### make sure dates and time are valid
 time_list <- check_valid_sixhr(start_date = start_date,
                                end_date = end_date,
                                start_hr = start_hr,
                                end_hr = end_hr)


 #### compute date-time list spanning both date times
 start_time <- time_list$start_time
 end_time <- time_list$end_time


 #### create list of query times from start time to end time
 dt <- data.table(pull_date = seq(start_time, end_time, by = repo_interval))

 parse_list <- c("filename_tag", "year", "month", "day", "hour")

 dt[, (parse_list) := list(filename_tag,
                           year(pull_date),
                           sprintf("%02d", month(pull_date)),
                           sprintf("%02d",lubridate::day(pull_date)),
                           sprintf("%02d",lubridate::hour(pull_date)))]

 dt[, filename := paste0(filename_tag,
                         year,
                         month,
                         day,
                         hour,
                         file_ext)]

return(dt)


}


chirpname_daily <- function(start_date,
                            end_date){

  #### makes sure dates and time are valid
  time_list <-
  check_valid_daily(start_date = start_date,
                    end_date = end_date)

  #### construst list of days between start and end date
  date_list <- seq(as.Date(start_date), as.Date(end_date), "days")

  construct_chirpname <- function(X){

    X <- gsub("-", ".", X)

    y <- paste("chirps-v2.0", X, "tif.gz", sep = ".")

  }

  dld_filelist <- unlist(lapply(date_list, construct_chirpname))

  return(dld_filelist)

  time_list <- check_valid_daily(start_date = start_date,
                                 end_date = end_date)

  #### create list of query times from start time to end time
  dt <- data.table(pull_date = seq(start_date,
                                   end_date,
                                   by = repo_interval))

  date_list <- seq(as.Date(time_list$start_date),
                   as.Date(time_list$end_date),
                   "days")

  construct_chirpname <- function(X){

    X <- gsub("-", ".", X)

    y <- paste("chirps-v2.0", X, "tif.gz", sep = ".")

  }

  dld_filelist <- lapply(date_list, construct_chirpname)

  return(dld_filelist)


}




check_valid_daily <- function(start_date,
                              end_date) {

  ### first make sure start_date and end_date are dates
  if (is.Date(start_date) == FALSE){
    stop("start_date argument is not a Date, did you specify it in the form as.Date('yyyy-mm-dd')")
  }

  if (is.Date(end_date) == FALSE){
    stop("end_date argument is not a Date, did you specify it in the form as.Date('yyyy-mm-dd')")
  }


  if(start_date > end_date) stop("Invalid time range, start time exceeds end time!")

  return(list(start_date = start_date,
              end_date = end_date))

}

check_valid_sixhr <- function(start_date,
                              end_date,
                              start_hr,
                              end_hr) {

  ### first make sure start_date and end_date are dates
  if (is.Date(start_date) == FALSE){
    stop("start_date argument is not a Date, did you specify it in the form as.Date('yyyy-mm-dd')")
  }

  if (is.Date(end_date) == FALSE){
    stop("end_date argument is not a Date, did you specify it in the form as.Date('yyyy-mm-dd')")
  }

  ### then make sure start_hr and end_hr are both numbers
  if (is.numeric(start_hr) == FALSE) stop("start_hr is not a numeric, please specify a positive number")

  if (is.numeric(end_hr) == FALSE) stop("end_hr is not a numeric, please specify a positive number")


  ### check that start_hr and end_hr are both positive numbers
  if (start_hr < 0) stop("start_hr must be greater than or equal to 0")

  if (end_hr < 0) stop("end_hr must be greater than or equal to 0")

  ### check that the start date and time precedes end date and time
  start_hr <- trunc(start_hr)
  end_hr <- ceiling(end_hr)

  std_time <- paste0(start_hr, ":00:00")

  start_time <- ymd_hms(paste(start_date, std_time, sep = " "))

  std_time <- paste0(end_hr, ":00:00")

  end_time <- ymd_hms(paste(end_date, std_time, sep = " "))

  if(start_time > end_time) stop("Invalid time range, start time exceeds end time!")

  start_time <- lubridate::floor_date(start_time,
                                      unit = "6 hours")

  end_time <- lubridate::ceiling_date(end_time,
                                      unit = "6 hours")



  ### creates the date-times
  return(list(start_time = start_time,
              end_time = end_time))

}



check_valid_daily <- function(start_date,
                              end_date) {

  ### first make sure start_date and end_date are dates
  if (is.Date(start_date) == FALSE){
    stop("start_date argument is not a Date, did you specify it in the form as.Date('yyyy-mm-dd')")
  }

  if (is.Date(end_date) == FALSE){
    stop("end_date argument is not a Date, did you specify it in the form as.Date('yyyy-mm-dd')")
  }

  if (end_date < start_date){
    stop("End Date should be after the start date")
  }


  ### creates the date-times
  return(list(start_time = start_time,
              end_time = end_time))

}






### download function for getting the actual files
download_worker <- function(dsn,
                            url){

  download.file(url = url,
                destfile = paste(dsn, basename(url), sep = "/"),
                mode = "wb")

  R.utils::gunzip(paste(dsn, basename(url), sep = "/"),
                  remove = TRUE,
                  overwrite = TRUE)

}

## check that the url actually exists
checkurl_exist <- function(url){

  HTTP_STATUS_OK <- 200

  hd <- httr::HEAD(url)

  status <- hd$all_headers[[1]]$status

  test_result <- list(exists = status == HTTP_STATUS_OK,
                      status = status)

  if (test_result$exists == FALSE) {
    message("WARNING: ", basename(url), " is not on the CHIRPS database")
  }

  return(test_result$exists)

}


################################################################################
#### functions for the monthly chirps pulls
chirpname_monthly <- function(start_date,
                              end_date,
                              repo_interval = "month",
                              filename_tag = "chirps-v2.0.",
                              file_ext = ".tif.gz"){

  time_list <- check_valid_month(start_date = start_date,
                                 end_date = end_date)

  #### create list of query times from start time to end time
  dt <- data.table(pull_date = seq(start_date,
                                   end_date,
                                   by = repo_interval))

  parse_list <- c("filename_tag", "year", "month")

  dt[, (parse_list) := list(filename_tag,
                            year(pull_date),
                            sprintf("%02d", month(pull_date)))]

  dt[,year_month := paste(year, month, sep = ".")]

  dt[, filename := paste0(filename_tag,
                          year_month,
                          file_ext)]

  return(dt)



}


check_valid_month <- function(start_date,
                              end_date) {

  ### first make sure start_date and end_date are dates
  if (is.Date(start_date) == FALSE){
    stop("start_date argument is not a Date, did you specify it in the form as.Date('yyyy-mm-dd')")
  }

  if (is.Date(end_date) == FALSE){
    stop("end_date argument is not a Date, did you specify it in the form as.Date('yyyy-mm-dd')")
  }


  if(start_date > end_date) stop("Invalid time range, start time exceeds end time!")

  ### put together the list of year-months to be pulled
  start_month <- lubridate::format_ISO8601(as.Date(start_date), precision = "ym")
  end_month <- lubridate::format_ISO8601(as.Date(end_date), precision = "ym")



  ### creates the date-times
  return(list(start_time = start_month,
              end_time = end_month))

}


#################################################################################
#### functions for the annual chirps pulls
chirpname_annual <- function(start_year,
                             end_year){

  if (is.integer(start_year) == FALSE | is.integer(end_year) == FALSE){
    stop("start and end year must be integers")
  }

  if (start_year > end_year) stop("Invalid time range, start year exceeds end year")

  return(list(start_year = start_year,
              end_year = end_year))


}
































