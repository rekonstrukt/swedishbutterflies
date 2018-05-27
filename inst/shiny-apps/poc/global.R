library(dplyr)

message("loading global.R")
pool <- swedishbutterflies:::sebms_connect()

onStop(function() {
  poolClose(pool)
})

enableBookmarking(store = "url")

