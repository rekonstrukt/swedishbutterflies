sebms_pool <- NULL

.onAttach <- function(lib, pkg){

  # echo "swedish" | toilet -f cricket
  # echo "butterflies" | toilet -f cricket
  
  welcome <-
"https://dagfjarilar.lu.se__ __         __    
.-----.--.--.--.-----.--|  |__|.-----.|  |--.
|__ --|  |  |  |  -__|  _  |  ||__ --||     |
|_____|________|_____|_____|__||_____||__|__|
 __           __   __               ___ __ __              
|  |--.--.--.|  |_|  |_.-----.----.'  _|  |__|.-----.-----.
|  _  |  |  ||   _|   _|  -__|   _|   _|  |  ||  -__|__ --|
|_____|_____||____|____|_____|__| |__| |__|__||_____|_____|
Use runShinyApp('poc') to run the Shiny proof-of-concept app"

  packageStartupMessage(welcome)
}

.onLoad <- function(libname, pkgname) {
  # NOTE: run tunnels.sh before establishing db connection
  # TODO set utf-8 on the postgres connection
  #conn <- poolCheckout(pool)
  #dbSendQuery(conn, "SET NAMES utf8;")
  # do something with conn
  #poolReturn(conn)  
  #}
  pool <- swedishbutterflies:::sebms_connect()
  if (is.null(pool)) 
    message("Please enable db connections (run tunnels?)")
  else
    sebms_pool <<- pool
}

.onDetach <- function(libpath) {
  
  message("Closing sebms connection pool")
  pool::poolClose(sebms_pool)
}

# .onUnload <- function(libpath) {
# 
#   message("Closing sebms connection pool")
#   pool::poolClose(sebms_pool)
#   
#   # kill_cons <- function () {
#   #  all_cons <- DBI::dbListConnections(pool)
#   #  message(all_cons)
#   #  purrr::walk(all_cons, dbDisconnect)
#   # }
# 
# }
