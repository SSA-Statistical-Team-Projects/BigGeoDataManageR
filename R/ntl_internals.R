################################################################################
########## INTERNAL FUNCTIONS FOR PULLING NIGHT LIGHT DATA FROM EOG ############
################################################################################

##### naming functions

name_daily_ntl <- function(start_date,
                           end_date,
                           satel_name,
                           cloud_mask = T
                           ){

  ###validate the inputs
  check_list <- check_dailyname_ntl(start_date = start_date,
                                    end_date = end_date,
                                    satel_name = satel_name,
                                    cloud_mask = cloud_mask)



  time_list <- seq(from = as.Date(start_date),
                   to = as.Date(end_date),
                   "days")

  dt <- data.table(day = time_list)

  dt[, c("ptype", "satellite", "cloud", "day") := list("SVDNB",
                                                       satel_name,
                                                       ifelse(cloud_mask == T, "vcld.tif", "rade9d.tif"),
                                                       paste0("d", gsub("-", "", day)))]



  dt[, full_link := paste(ptype, satel_name, day, sep = "_")]

  dt[, full_link := paste(full_link, cloud, sep = ".")]

  return(dt$full_link)


}






check_dailyname_ntl <- function(start_date,
                                end_date,
                                satel_name,
                                cloud_mask){

  check_valid_daily(start_date = start_date,
                    end_date = end_date)

  ## make sure proper satellite name is used

  if(sum(grepl(satel_name, c("npp", "j01"))) < 1) stop("Wrong satellite, please select j01 or npp")

  ## check whether cloud mask argument is a logical

  if(is.logical(cloud_mask) == FALSE) stop("Please select TRUE or FALSE on cloud_mask arg")


}



ntl_downloader <- function(client_id,
                           client_secret,
                           username,
                           password,
                           grant_type,
                           token_url,
                           data_url){

  params <- list(client_id = client_id,
                 client_secret = client_secret,
                 username = username,
                 password = password,
                 grant_type = grant_type)

  response <- httr::POST(token_url,
                         body = params,
                         encode = "form")

  access_token_list <- jsonlite::fromJSON(httr::content(response,
                                                        as = "text",
                                                        encoding = "UTF-8"))

  access_token <- access_token_list$access_token
  # Submit request with token bearer and write to output file
  ## Change data_url variable to the file you want to download

  auth <- paste('Bearer', access_token)
  ## You can either define the output file name directly
  # output_file <- 'EOG_sensitive_contents.txt'
  ## Or get the filename from the data_url variable

  output_file <- basename(data_url)

  download.file(data_url,
                output_file,
                mode = "wb",
                headers = list(Authorization = auth))

}




##### internal functions to help pull the monthly notile data
construct_month_link <- function(year,
                                 month,
                                 version,
                                 slc_type,
                                 no_tile,
                                 link_base = "https://eogdata.mines.edu/nighttime_light"){


  slc_type <- match.arg(slc_type, c("vcmcfg", "vcmslcfg"))

  tile_type <- ifelse(no_tile == TRUE, "monthly_notile", "monthly")

  year <- as.integer(year)

  month <- sprintf("%02d", as.integer(month))

  ### construct link
  url_link <- paste(link_base,
                    tile_type,
                    version,
                    year,
                    paste0(year, month),
                    slc_type,
                    "",
                    sep = "/")

  return(url_link)

}




valid_url <- function(url,
                      t=2){
  con <- url(url)

  check <- suppressWarnings(try(open.connection(con,
                                                open = "rt",
                                                timeout = t),
                                silent=T)[1])

  suppressWarnings(try(close.connection(con),
                       silent=T))

  result <- ifelse(is.null(check),
                   TRUE,
                   FALSE)


  return(result)

}



#### internal functions to help pull the annual data

construct_year_link <- function(year,
                                version,
                                link_base = "https://eogdata.mines.edu/nighttime_light/annual/"){

  year <- as.integer(year)

  url_link <- paste0(link_base,
                     version,
                     "/",
                     year)

  return(url_link)

}




























































