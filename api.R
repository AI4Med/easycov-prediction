library(plumber)
r <- plumb('plumber.R')
r$run(port=8082, host='0.0.0.0')