
#' Run leafviz
#'
#' @param infile
#'
#' @return
#' @export
#'
#' @examples
leafviz <- function(infile = system.file("extdata/Brain_vs_Heart_results.Rdata", package = "leafviz") ){

  stopifnot(file.exists(infile))
  message(" * Welcome to Leafviz! Loading data...")
  load(infile, envir = .GlobalEnv)
  message(" * running app!")
  # if using Rstudio then getOption("browser") is a function for some reason
  if(!is.function(getOption("browser") ) ){
    # if using windows - default to firefox for now
    if (getOption("browser") == ""){
        options(browser = "firefox")
    }
  }
  shiny::runApp( launch.browser=TRUE, appDir = system.file("application", package = "leafviz") )

}
