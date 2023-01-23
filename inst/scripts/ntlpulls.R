library(httr)
library(jsonlite)
library(utils)
# Retrieve access token
params <- list(
  client_id = 'eogdata_oidc',
  client_secret = '2677ad81-521b-4869-8480-6d05b9e57d48',
  username = "iedochie@worldbank.org",
  password = "I651ny9@1588",
  grant_type = 'password'
)
token_url <- 'https://eogauth.mines.edu/auth/realms/master/protocol/openid-connect/token'
response <- httr::POST(token_url, body = params, encode = "form")
access_token_list <- jsonlite::fromJSON(httr::content(response,as="text",encoding="UTF-8"))
access_token <- access_token_list$access_token
# Submit request with token bearer and write to output file
## Change data_url variable to the file you want to download
data_url <- "https://eogdata.mines.edu/nighttime_light/nightly/cloud_cover/SVDNB_npp_d20120301.vcld.tif"
auth <- paste('Bearer', access_token)
## You can either define the output file name directly
# output_file <- 'EOG_sensitive_contents.txt'
## Or get the filename from the data_url variable
output_file <- basename(data_url)
download.file(data_url,output_file,mode = "wb", headers = list(Authorization = auth))



