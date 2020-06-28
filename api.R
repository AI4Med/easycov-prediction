library(plumber)
r <- plumber::plumb("predict.R")
r$run(port=8082, host='0.0.0.0')