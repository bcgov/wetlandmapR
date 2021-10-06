# SCRIPT TO DONWLOAD CLASSIFIED RASTER FROM ECOCAT 

# 3 Category
url <- "http://a100.gov.bc.ca/appsdata/acat/documents/r58802/3CategoryPrediction_1595014314939_5010738801.zip"
download.file(url, destfile="data/out/3Category.zip",method="libcurl")
unzip(zipfile = "data/out/3Category.zip", exdir = "data/out")

# 10 Category
url <- "http://a100.gov.bc.ca/appsdata/acat/documents/r58802/10CategoryPrediction_1595014787935_5010738801.zip"
download.file(url, destfile="data/out/10Category.zip",method="libcurl")
unzip(zipfile = "data/out/10Category.zip", exdir = "data/out")
