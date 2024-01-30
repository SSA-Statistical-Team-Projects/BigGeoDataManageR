#' Automate the process of pulling building data from the World Pop website
#'
#' @param iso The standard ISO-3 country code for country of interest
#' @param wpversion The World Pop version of interest (use wpopbuilding_vcheck function to check version availability)
#' @param ldrive_dsn Destination for the downloaded files
#'
#'
#' @details For more information on documentation please run wpopbuilding_pull(iso = "XXX", wpversion, ldrive_dsn). This should
#' download the documentation complete with data file descriptions and citation references
#'
#' @export
#'
#' @import data.table
#' @importFrom RCurl getURL

wpopbuilding_pull <- function(iso = "BEN",
                              wpversion = "v2.0",
                              ldrive_dsn = "data-raw"){


  #construct the link to the world pop website with the building links
  url <- paste("ftp://ftp.worldpop.org/repo/wopr/_MULT/buildings", wpversion, "", sep = "/")

  #list the file names present on the specific version sub-website for world pop building
  filenames <- getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  filenames <- strsplit(filenames, "\r\n")
  filenames <- unlist(filenames)

  filenames <- as.data.table(filenames)
  filenames[,country := substr(filenames, 1, 3)]

  #return a message if country not on the list was selected, otherwise go ahead and download data and store locally
  if((iso %in% filenames[,country]) == FALSE){

    return("this country is not within this database")

  } else {

    rel_fn <- filenames[country == iso,filenames]
    url <- paste(url, rel_fn, sep = "")

    return(download.file(url = url, destfile = paste(ldrive_dsn, rel_fn, sep = "/")))

  }


}


#' This function returns a datatable of all available versions of World Pop raster
#' building data in each country with available
#'
#' simply run : wpopbuilding_check() in your console.
#'
#' @note ALWAYS GIVE AT LEAST 2 MINUTES BETWEEN SUCCESSIVE RUNS OF THIS FUNCTION. THE PREVENTS SUBSEQUENT REQUESTS ON ITS
#' SERVER WITHIN THAT TIME INTERVAL, SO YOU WILL GET A <NOT SET> FUNCTION ERROR.
#'
#' @return a data.table object with list and versions of building data
#'
#' @import data.table
#' @importFrom RCurl getURL
#' @export

wpopbuilding_vcheck <- function(){

  building_link <- "https://data.worldpop.org/repo/wopr/_MULT/buildings/"

  #construct the link to the world pop website with the building links
  #list the file names present on the specific version sub-website for world pop building
  find_sublinks <- function(url = building_link){

    versions <- RCurl::getURL(url = building_link,
                              ftp.use.epsv = FALSE,
                              dirlistonly = TRUE)
    versions <- strsplit(versions, "\r\n")
    versions <- unlist(versions)

    return(versions)

  }

  bld_vers <- find_sublinks() #function execution

  find_blddt <- function(versions){

    ##construct link
    suburl <- paste(building_link, versions, "", sep = "/")

    filenames <- find_sublinks(url = suburl)
    filenames <- data.table::as.data.table(filenames)
    filenames[,country := substr(filenames, 1, 3)]
    filenames[,versionnum := versions]

    return(filenames)
  }



  all_filenames <- data.table::rbindlist(lapply(bld_vers, find_blddt))

  return(all_filenames)

}



