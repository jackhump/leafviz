
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
  load(infile, envir = .GlobalEnv)
  if (getOption("browser") == ""){
    options(browser = "firefox")
  }

  shiny::runApp( launch.browser=TRUE, appDir = system.file("application", package = "leafviz") )

}
