################################################################################
#                                                                              #
#             SCRIPT TO DOWNLOAD RAW  DATA FROM WORLD POP                      #
#                                                                              #
################################################################################

#First, load packages
if (Sys.info()[["user"]] == "wb570371"){

  .libPaths("E:/Daylan/R")

}

#you may need to get the wopr from teh following code:

#  devtools::install_github('wpgp/wopr')


packages <- c("wopr", "sf","sp")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


##Get the worldpop  catalog to see the country codes and data avaiable.
catalogue <- getCatalogue()
View(catalogue)

#Subset the data that you want to download from worldpop
BFA_agesex<- subset(catalogue, country=='BFA' & category=='Population'&
                version=='v1.0' &
                  filetype=='agesex')

#Download the data. By default, this function wont download data bigger than 100mb
#Must specify the the size of the file.
#I used a temporary location for the files to be saved.
#It creates a folder with the country code.
downloadData(dat= BFA_agesex,
             wopr_dir="//cwapov/cwapov/BFA/GEO/Population",
             maxsize = 1000)




