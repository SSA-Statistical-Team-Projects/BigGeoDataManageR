#' A function to download the rainfall data from the CHIRPS database
#'
#' @param link_base the link to the daily chirps data (no need to change this)
#' @param folder_name subfolder within `link_base` of interest
#' @param start_date date at the beginning of period of interest
#' @param end_date the last day of the period of interest
#' @param exdir the external directory to store downloaded rainfall rasters
#' @param tif_type the name of the file extension used (whether tifs or tiffs)
#' @param res_folder the resolution of rainfall data pulled (options are p05 and p25)
#' @param chirps_version the data version (default is the latest available i.e. version 2)
#' @param decomp_tfgiz if TRUE, the tfgiz will be decompressed into a tif file
#' @param remove_original if TRUE, original downloaded tif.gz file will be deleted as decompression to tif
#'
#' @importFrom R.utils gunzip
#' @importFrom lubridate year
#' @importFrom stringr str_extract
#' @export

download_dailychirps <- function(link_base = "https://data.chc.ucsb.edu/products/CHIRPS-2.0/",
                                 folder_name = "africa_daily",
                                 start_date,
                                 end_date,
                                 exdir,
                                 tif_type = "tifs",
                                 res_folder = "p05",
                                 chirps_version = "chirps-v2.0",
                                 decomp_tfgiz = FALSE,
                                 remove_original = FALSE){

  date_list <- seq(as.Date(start_date), as.Date(end_date), "days")

  construct_chirpname <- function(X){

    X <- gsub("-", ".", X)

    y <- paste(chirps_version, X, "tif.gz", sep = ".")

  }

  dld_filelist <- lapply(date_list, construct_chirpname)


  ###construct full path
  construct_chirplink <- function(X){

    get_date <- str_extract(X, "[0-9]{4}.[0-9]{2}.[0-9]{2}")
    get_date <- gsub("\\.", "-", get_date)
    get_date <- as.Date(get_date)

    link_year <- year(get_date)

    url <- paste(link_base, folder_name, tif_type, res_folder, link_year, X, sep = "/")

    return(url)

  }

  url_list <- lapply(dld_filelist, construct_chirplink)
  url_list <- unlist(url_list)

  download_worker <- function(X){

    y <- substr(X, nchar(X) - 28, nchar(X))

    download.file(url = X,
                  destfile = paste(exdir, y, sep = "/"),
                  mode = "wb")

    if (decomp_tfgiz == TRUE){

      R.utils::gunzip(paste(exdir, y, sep = "/"), remove = remove_original)

    }

  }

  lapply(url_list, download_worker)


}



