################################################################################
########## INTERNAL FUNCTIONS FOR PULLING NIGHT LIGHT DATA FROM EOG ############
################################################################################

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



  dt[, full_link := paste(ptype, satel_name, day, cloud, sep = "_")]

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
