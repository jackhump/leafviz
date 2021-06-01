
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
make_report <- function(
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


#' Write tables as TSV
#'
#' @param infile
#' @param outfile
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom magrittr "%>%"
export_tables <- function(infile =  system.file("extdata/Brain_vs_Heart_results.Rdata", package = "leafviz"), outfolder = file.path(getwd(), "leafviz_tables" ) ){
  if( !dir.exists(outfolder)){dir.create(outfolder)}

  message(" * Loading data...")
  load(infile, envir = .GlobalEnv)
  message(" * Writing tables to ", outfolder)

  # Clusters
  # un-italicise gene name
  cluster_df <- clusters %>% dplyr::mutate(gene = gsub("<i>|</i>", "", gene, fixed = FALSE))

  cluster_out <- file.path(outfolder, paste0(code, "_sig_clusters.tsv") )

  write.table(cluster_df, file = cluster_out, sep = "\t", quote = FALSE, row.names = FALSE)

  # Introns
  intron_df <- introns %>%
    mutate(coord = paste0(chr, ":", start, "-", end)) %>%
    mutate(ensemblID = gsub("_[0-9]$", "", ensemblID) ) %>%
    select(clusterID, gene, ensemblID, coord, verdict, deltaPSI = deltapsi)

  intron_out <- file.path(outfolder, paste0(code, "_sig_introns.tsv") )

  write.table(intron_df, file = intron_out, sep = "\t", quote = FALSE, row.names = FALSE)

}
