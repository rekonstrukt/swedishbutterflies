library(dplyr)
library(pool)
library(swedishbutterflies)

message("loading global.R")
pool <- swedishbutterflies:::sebms_connect()
swedishbutterflies:::sebms_assert_connection(pool)
#onStop(function() {
#  poolClose(pool)
#})

enableBookmarking(store = "url")

