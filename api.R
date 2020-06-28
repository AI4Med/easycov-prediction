# library(plumber)
list.of.packages <- c("plumber")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

r <- plumber::plumb("plumber.R")
r$run(port=8082, host='0.0.0.0')