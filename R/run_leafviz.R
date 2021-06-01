
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
    if (is.null(getOption("browser") ) ){
        options(browser = "firefox")
    }
  }
  shiny::runApp( launch.browser=TRUE, appDir = system.file("application", package = "leafviz") )

}

#' Write simple HTML report
#'
#' @param infile
#' @param outfile
#'
#' @return
#' @export
#'
#' @examples
make_leafviz_report <- function(
  infile =  system.file("extdata/Brain_vs_Heart_results.Rdata", package = "leafviz"),
  outfile = file.path(getwd(), "report.html" ) ) {

  if( !file.exists(infile)){
    stop(" * infile doesn't exist")
  }

  if( !dir.exists(dirname(outfile))){
    stop(" * path to outfile doesn't exist")
  }

  message(" * Loading data...")
  load(infile, envir = .GlobalEnv)
  message(" * Writing report to ", outfile)
  # translate tables into HTML
  report <- c( paste("<h1>", code, "</h1>" ) ,
               stargazer::stargazer(sample_table,type = "html", summary = FALSE, rownames = FALSE,
                                    title = "Samples" ),
               "<p>",
               stargazer::stargazer(intron_summary,type = "html", summary = FALSE, rownames = FALSE,
                                    title = "Junction classification"),
               "<p>",
               stargazer::stargazer(cluster_summary,type = "html", summary = FALSE, rownames = FALSE,
                                    title = "Cluster classification")
  )
  writeLines(report,con = outfile)
}
