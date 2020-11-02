
#############
# SHINY APP
#############

#' shiny server
#'
#' @param input
#' @param output
#' @param session
#'
#' @return
#' @export
#'
#' @import shiny
#' @examples
server <- function(input, output, session) {

  #load(infile)

  output$logo <- renderImage({
    list( src = leafcutter_logo, alt = "", width = "15%", height = "15%" )
  }, deleteFile = FALSE)

  output$all_clusters <- DT::renderDataTable({
    DT::datatable( clusters[,c("gene","coord","N","FDR","annotation")],
              escape = FALSE,
              rownames = FALSE,
              colnames = c('Genomic location'='coord','Gene'='gene','N'='N','Annotation'='annotation','q'='FDR'),
              selection = 'single',
              caption = "Click on a row to plot the corresponding visualization. N: number of introns within a cluster. q: Benjamini–Hochberg q-value.",
              fillContainer = FALSE,
              options = list(
                pageLength = 15,
                columnDefs = list(list(className = 'dt-center', targets = 0:4) )
              )
              )
  })
  output$sample_table <- DT::renderDataTable({
    DT::datatable(sample_table,
              escape = FALSE,
              rownames = FALSE,
              fillContainer = FALSE,
              options <- list( searching = FALSE, paging = FALSE, info = FALSE )
              )
  })

  # observeEvent(input$toggleInstruct, {
  #   renderUI()
  # })

  shinyjs::onclick("welcome", toggle(id = "popupInstruct", anim = TRUE) )

  observeEvent( input$aboutLink, {
    updateTabsetPanel(session, "navBarPage", selected = "About")
  })

  observeEvent( input$aboutLink2, {
    updateTabsetPanel(session, "navBarPage", selected = "About")
  })

  # SUMMARY - does this need to be in server and not just UI?

  output$experimentCode <- renderText({
    paste("Experiment code:", code )
  })
  output$annotationCode <- renderText({
    paste("Annotation source:", basename(annotation_code) )
  })
  output$cluster_summary <- DT::renderDataTable({
    DT::datatable(cluster_summary,
              escape = FALSE,
              rownames = FALSE,
              fillContainer = FALSE,
              options <- list( searching = FALSE, paging = FALSE, info = FALSE )
              )
  })

  output$intron_summary <- DT::renderDataTable({
    DT::datatable(intron_summary,
              escape = FALSE,
              rownames = FALSE,
              fillContainer = FALSE,
              options <- list( searching = FALSE, paging = FALSE, info = FALSE )
              )
  })

  # TABLES

  output$cluster_view = DT::renderDataTable({
    clu <- mydata()$cluster
    if(!is.null(clu)){
      if(length(introns)){
        DT::datatable( filter_intron_table(introns, clu, toSave=FALSE),
                 autoHideNavigation = TRUE, rownames = TRUE,
                 options <- list( searching = FALSE, paging = FALSE, info = FALSE)
          )
      }
    }else{
      print("no cluster selected!")
    }

  })

  # SET REACTIVE VALUE WITH A DEFAULT
  defaultValue <- 1
  values <- reactiveValues(default = defaultValue) # RBFOX1 in the Brain vs Heart dataset
  # REACTIVE VALUE IS UPDATED BY INPUT
  observeEvent(input$all_clusters_rows_selected,{
    #print("new row selected!")
    values$default <- input$all_clusters_rows_selected # if all_clusters_rows_selected changes then update value - this sets everything!
    #print(paste0("VALUE: ", values$default ))
  })

  # USE REACTIVE VALUE TO GENERATE ALL VARIABLES NEEDED

  mydata <- eventReactive(values$default,{
    sel <- values$default
    gene  <- clusters[ sel, ]$gene
    gene <- gsub("<.*?>", "", gene) # strip out html italic tags
    width <- leafviz::getGeneLength(exons_table, gene)
    clusterID <- clusters[ sel, ]$clusterID
    coord <- clusters[ sel, ]$coord
    return(list(gene = gene, width = width, cluster = clusterID, coord = coord) )
  })


  # PLOTTING

  output$select_cluster_plot <- renderPlot({
    plotTitle <- c(mydata()$gene, as.character(mydata()$cluster) )
    suppressWarnings( print(
      make_cluster_plot( mydata()$cluster,
                       main_title = plotTitle,
                       meta = meta,
                       cluster_ids = cluster_ids,
                       exons_table = exons_table,
                       counts = counts,
                       introns = introns)
    ))
  }, width = "auto", height = "auto",  res = 90
  )

  observeEvent(values$default,{
    output$select_gene_plot <- renderPlot({
    suppressWarnings( print(
      make_gene_plot(mydata()$gene, counts = counts, introns = introns, exons_table = exons_table, cluster_list = clusters, clusterID = mydata()$clusterID, introns_to_plot = introns_to_plot, debug=F)
      )
    )
    }, width = mydata()$width, height = "auto", res = 90 # try changing height param
   )
  })

  # TITLES

  output$gene_title <- renderText({
    return( as.character( mydata()$gene )  )
  })

  output$cluster_title <- renderText({
    return( as.character(mydata()$cluster))
  })

  # DOWNLOAD HANDLING

  output$downloadClusterPlot <- downloadHandler(
    filename = function() { paste0(mydata()$gene,"_", mydata()$cluster, '.pdf') },
    content = function(file) {
      plotTitle <- c(mydata()$gene, as.character(mydata()$cluster) )
      ggplot2::ggsave(file,
             plot = make_cluster_plot( mydata()$cluster,
                                  main_title = plotTitle,
                                  meta = meta,
                                  cluster_ids = cluster_ids,
                                  exons_table = exons_table,
                                  counts = counts,
                                  introns = introns),
             device = "pdf", width = 10, height = 5 )
    }
  )

  output$downloadClusterPlotWithTable <- downloadHandler(
    filename = function() { paste0(mydata()$gene,"_", mydata()$cluster, '_table.pdf') },
    content = function(file) {
      plotTitle <- c(mydata()$gene, as.character(mydata()$cluster ) )
      clusterPlot <- make_cluster_plot( mydata()$cluster,
                                        main_title = plotTitle,
                                        meta = meta,
                                        cluster_ids = cluster_ids,
                                        exons_table = exons_table,
                                        counts = counts,
                                        introns = introns)
      # make table theme
      tableTheme <- gridExtra::ttheme_minimal(
        core=list(bg_params = list(fill = c("whitesmoke","white"), col=NA)
        ),
        colhead=list(fg_params=list(col="black", fontface="bold"),
                     bg_params = list(fill="white")),
        rowhead=list(fg_params=list(col="black"),
                     bg_params = list(fill=c("white", "whitesmoke"))))

      mytable <- gridExtra::tableGrob(filter_intron_table(introns, mydata()$cluster, toSave=TRUE), theme = tableTheme )
      mycols <- ncol(mytable)
      mytable$widths <- unit( c( 1/(3*mycols), rep(1/mycols, mycols-1) ), "npc")

      mytable <- gtable::gtable_add_grob(mytable,
                           grobs = segmentsGrob( # line across the bottom
                             x0 = unit(0,"npc"),
                             y0 = unit(0,"npc"),
                             x1 = unit(1,"npc"),
                             y1 = unit(0,"npc"),
                             gp = gpar(lwd = 2.0)),
                           t = 2, b = nrow(mytable), l = 1, r = mycols)

      mytable <- gtable::gtable_add_grob(mytable,
                                 grobs = segmentsGrob( # line across the bottom
                                   x0 = unit(0,"npc"),
                                   y0 = unit(0,"npc"),
                                   x1 = unit(1,"npc"),
                                   y1 = unit(0,"npc"),
                                   gp = gpar(lwd = 2.0)),
                                 t = 1, b = 1, l = 1, r = mycols)

      ggplot2::ggsave(file, plot = grid.arrange(clusterPlot, mytable, nrow =2),
           device = "pdf", width = 10, height = 8 )
    }
  )

  output$downloadGenePlot <- downloadHandler(
    filename = function() { paste( mydata()$gene,"_","allClusters", '.pdf', sep='') },
    content = function(file) {
      ggplot2::ggsave(file,
             plot = make_gene_plot(mydata()$gene, counts = counts, introns = introns, exons_table = exons_table, cluster_list = clusters, clusterID = NULL, introns_to_plot = introns_to_plot),
             device = "pdf", width = ifelse( mydata()$width == "auto", yes = 10, no = mydata()$width / 100 ), height = 6, units = "in", limitsize = FALSE)
    }
  )

  # UCSC LINKS
  output$viewClusterUCSC <- renderUI({
    coord <- mydata()$coord
    db <- strsplit(basename(annotation_code), split = "_")[[1]][2]
    # guess species from genome build - if not possible then leave blank.
    org <- NULL
    if( grepl("hg", db)){ org <- "human"}
    if( grepl("mm", db)){ org <- "mouse"}
    if( is.null(org)){
      orgChoice <- ""
    }else{
      orgChoice <- paste0("&org=",org)
    }

    # zoom out the position a bit
    chr <- strsplit(coord, ":")[[1]][1]
    start <- as.numeric(strsplit( strsplit(coord, ":")[[1]][2], "-" )[[1]][1])
    end <- as.numeric(strsplit( strsplit(coord, ":")[[1]][2], "-" )[[1]][2])
    start <- start - 100
    end <- end + 100
    coord <- paste0(chr, ":", as.character(start), "-", as.character(end))
    url <- paste0( "http://genome.ucsc.edu/cgi-bin/hgTracks?", orgChoice, "&db=",db,"&position=", coord )
    return(tags$a(href = url, "view on UCSC", target = "_blank", class = "btn btn_default", id = "UCSC" ) )
    })

  output$viewGeneUCSC <- renderUI({
    db <- strsplit(basename(annotation_code), split = "_")[[1]][2]
    org <- NULL
    if( grepl("hg", db)){ org <- "human"}
    if( grepl("mm", db)){ org <- "mouse"}
    if( is.null(org)){
      orgChoice <- ""
    }else{
      orgChoice <- paste0("&org=",org)
    }
    gene <- mydata()$gene
    url <- paste0( "http://genome.ucsc.edu/cgi-bin/hgTracks?",orgChoice,"&db=",db,"&singleSearch=knownCanonical&position=", gene)
    return(tags$a(href = url, "view on UCSC", target = "_blank", class = "btn btn_default", id = "UCSC" ) )
  })

  #### PCA
  #names(pca[[1]]) <- gsub(" ", "_", names(pca[[1]]))
  output$pca_choices <- renderUI({
    choices <- names(pca[[1]])[ grepl("^PC[0-9]", names(pca[[1]])) ] # find all PCs
    selectInput( inputId = "first_PC", label = "First principal component", choices = choices, selected = choices[1]  )
  })
  output$pca_colour_choices <- renderUI({
    choices <- names(pca[[1]])[ !grepl("^PC[0-9]", names(pca[[1]])) ]
    selectInput( inputId = "colour_choice", label = "Colour points by", choices = choices, selected = choices[1]  )
  })
  output$pca_shape_choices <- renderUI({
    choices <- names(pca[[1]])[ !grepl("^PC[0-9]", names(pca[[1]])) ]
    selectInput( inputId = "shape_choice", label = "Shape points by", choices = choices, selected = choices[1]  )
  })


  output$pca_plot <- renderPlot({
    if( ! is.null( input$first_PC)){
      createPCAPlot(input)
    }
  }, width = "auto", height ="auto", res = 90)

  output$downloadPCAPlot <- downloadHandler(
    filename = function() { paste0('PCA.pdf') },
    content = function(file) {
    ggsave(file, plot = createPCAPlot(), device = "pdf", width = 7, height = 7 )
  })

}
