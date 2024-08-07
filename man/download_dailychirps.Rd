% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bgdm_downloadchirps.R
\name{download_dailychirps}
\alias{download_dailychirps}
\title{A function to download the rainfall data from the CHIRPS database}
\usage{
download_dailychirps(
  link_base = "https://data.chc.ucsb.edu/products/CHIRPS-2.0/",
  folder_name = "africa_daily",
  start_date,
  end_date,
  exdir,
  tif_type = "tifs",
  res_folder = "p05",
  chirps_version = "chirps-v2.0",
  decomp_tfgiz = FALSE,
  remove_original = FALSE
)
}
\arguments{
\item{link_base}{the link to the daily chirps data (no need to change this)}

\item{folder_name}{subfolder within \code{link_base} of interest}

\item{start_date}{date at the beginning of period of interest}

\item{end_date}{the last day of the period of interest}

\item{exdir}{the external directory to store downloaded rainfall rasters}

\item{tif_type}{the name of the file extension used (whether tifs or tiffs)}

\item{res_folder}{the resolution of rainfall data pulled (options are p05 and p25)}

\item{chirps_version}{the data version (default is the latest available i.e. version 2)}

\item{decomp_tfgiz}{if TRUE, the tfgiz will be decompressed into a tif file}

\item{remove_original}{if TRUE, original downloaded tif.gz file will be deleted as decompression to tif}
}
\description{
A function to download the rainfall data from the CHIRPS database
}
