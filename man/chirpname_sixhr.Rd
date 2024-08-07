% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chirps_internals.R
\name{chirpname_sixhr}
\alias{chirpname_sixhr}
\title{Internal functions}
\usage{
chirpname_sixhr(
  start_date,
  end_date,
  start_hr,
  end_hr,
  repo_interval = "6 hours",
  filename_tag = "rfe_gdas.bin.",
  file_ext = ".gz"
)
}
\arguments{
\item{start_date}{An object of class date, must be specified like as.Date("yyyy-mm-dd")}

\item{end_date}{An object of class date, must be specified like as.Date("yyyy-mm-dd")}

\item{start_hr}{An integer, the starting hour in the interval for pulling the data}

\item{end_hr}{An integer, the final hour in the interval for pulling the data}
}
\description{
Internal functions
}
