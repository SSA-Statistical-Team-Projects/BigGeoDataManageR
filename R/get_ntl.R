#' Download Daily Night Time Lights Data
#'
#' Functions to download night time lights data from NASA's Earth Observation
#' Group's (EOG) database. The downloaded rasters are collected for different
#' time intervals, daily, monthly and annually.
#'
#' @param start_date An object of class date, must be specified like as.Date("yyyy-mm-dd")
#' @param end_date An object of class date, must be specified like as.Date("yyyy-mm-dd")
#' @param link_base the link to the daily chirps data (no need to change this)
#' @param username A character, username on NASA's Earth Observation Group database
#' @param password A character, password on NASA's Earth Observation Gruop database
#' @param satel_name A character, for the satellite name, options are 'j01' and 'npp'
#' @param cloud_mask A logical, if TRUE the cloud masked data is pulled, otherwise rade9d data is downloaded
#' @param cores An integer, the number of cores to be used in parallel downloading
#'
#' @importFrom parallel stopCluster makePSOCKcluster parLapply
#' @importFrom XML readHTMLTable
#' @importFrom httr content GET
#' @importFrom data.table as.data.table
#'
#' @export
#'


get_daily_ntl <- function(username,
                          password,
                          start_date,
                          end_date,
                          link_base = "https://eogdata.mines.edu/nighttime_light/nightly",
                          satel_name,
                          cloud_mask = T,
                          cores = 1L){


  ### first create the filename list
  name_list <- name_daily_ntl(start_date = start_date,
                              end_date   = end_date,
                              satel_name = satel_name,
                              cloud_mask = cloud_mask)


  ### create the full link set
  name_dt <- data.table(filename = name_list,
                        satellite = satel_name,
                        cloud_mask = ifelse(cloud_mask == TRUE,
                                            "cloud_cover",
                                            "rade9d"))

  name_dt[, folder := ifelse(satellite == "j01", "_j01", "")]
  name_dt[, folder := paste0(cloud_mask, folder)]


  name_dt[, full_link := paste(link_base, folder, filename, sep = "/")]


  ### call function to perform check that url exists and then run the download
  cl <- makePSOCKcluster(cores)
  on.exit(stopCluster(cl))

  ## check that the link exists
  name_dt$exist_status <- unlist(parLapply(cl = cl,
                                           X = name_dt$full_link,
                                           fun = checkurl_exist))

  ## download the chirps
  parallel::parLapply(cl = cl,
                      X = name_dt[exist_status == TRUE, full_link],
                      fun = ntl_downloader,
                      username = username,
                      password = password,
                      client_id = 'eogdata_oidc',
                      client_secret = '2677ad81-521b-4869-8480-6d05b9e57d48',
                      grant_type = 'password',
                      token_url = 'https://eogauth.mines.edu/auth/realms/master/protocol/openid-connect/token')


  return(name_dt[,c("full_link", "exist_status"), with = F])

}


get_month_ntl <- function(username,
                          password,
                          year,
                          month,
                          version,
                          no_tile = TRUE,
                          slc_type = c("vcmcfg", "vcmslcfg"),
                          indicator = c("avg_rade9h", "cf_cvg", "cvg"),
                          link_base = "https://eogdata.mines.edu/nighttime_light",
                          cores = 1L){


  ### create the full link
  url_link <- construct_month_link(year = year,
                                   month = month,
                                   version = version,
                                   slc_type = slc_type,
                                   no_tile = no_tile)


  if (no_tile == TRUE) {

    indicator <- match.arg(indicator, c("avg_rade9h", "cf_cvg", "cvg"))

  }


  cl <- makePSOCKcluster(cores)
  on.exit(stopCluster(cl))


  ### find the list of files in the url
  link_check <- valid_url(url_link)

  if (link_check == TRUE) {

    file_list <- as.data.table(readHTMLTable(content(GET(url_link), "text"))[[1]])

    file_list <- file_list[!grepl("tif.gz", Name),]


    if (no_tile == TRUE) {

      file_list <- file_list[grepl(indicator, Name), Name]

    }

    file_list <- unlist(lapply(X = url_link,
                               FUN = paste0,
                               file_list))

    parallel::parLapply(cl = cl,
                        X = file_list,
                        fun = ntl_downloader,
                        username = username,
                        password = password,
                        client_id = 'eogdata_oidc',
                        client_secret = '2677ad81-521b-4869-8480-6d05b9e57d48',
                        grant_type = 'password',
                        token_url = 'https://eogauth.mines.edu/auth/realms/master/protocol/openid-connect/token')

  }


}



get_annual_ntl <- function(year,
                           version,
                           link_base = "https://eogdata.mines.edu/nighttime_light/annual/"){



}






















































