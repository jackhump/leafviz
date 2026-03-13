#' Convert a GTF file to leafviz annotation files
#'
#' @description
#' Parses a GTF annotation file and writes the four annotation files required
#' by \code{\link{prepare_results}}: a gzipped exon table and three gzipped
#' BED files describing all introns, 5' splice sites, and 3' splice sites
#' derived from the transcript models in the GTF.
#'
#' @details
#' \strong{Output files}\cr
#' Four gzipped files are written, all sharing the same prefix:
#' \itemize{
#'   \item \code{<prefix>_all_exons.txt.gz} — tab-separated table with a
#'     header row (\code{chr}, \code{start}, \code{end}, \code{strand},
#'     \code{gene_name}), one row per unique exon.
#'   \item \code{<prefix>_all_introns.bed.gz} — BED-like file (no header),
#'     one row per intron per transcript, with columns
#'     \code{chr}, \code{start}, \code{end}, \code{gene_name},
#'     \code{gene_id}, \code{strand}, \code{transcript_id},
#'     \code{intron_id}, \code{biotype}, \code{tag}.
#'   \item \code{<prefix>_fiveprime.bed.gz} — same columns as above, one row
#'     per 5' splice site (\code{[intron_start, intron_start + 1]}).
#'   \item \code{<prefix>_threeprime.bed.gz} — same columns as above, one row
#'     per 3' splice site (\code{[intron_end, intron_end + 1]}).
#' }
#'
#' \strong{GTF compatibility}\cr
#' Tested with GENCODE and Ensembl GTF formats. Biotype is taken from the
#' first available of \code{transcript_type}, \code{gene_type}, or
#' \code{gene_biotype}. Gene name falls back to \code{gene_id} if
#' \code{gene_name} is absent. Multi-valued attributes (e.g. \code{tag}) are
#' collapsed with \code{|}. Exons whose \code{chr}, \code{strand},
#' \code{gene_name}, or \code{gene_id} are inconsistent with the first exon
#' of their transcript are silently skipped (matching the behaviour of the
#' original \code{gtf2leafcutter.pl} Perl script).
#'
#' \strong{Coordinate convention}\cr
#' Intron coordinates follow the leafcutter convention:
#' \code{start} = last base of the upstream exon,
#' \code{end} = first base of the downstream exon.
#' This matches leafcutter1 junction files directly. For leafcutter2 junction
#' files (where \code{end} is one position lower), \code{\link{prepare_results}}
#' applies the offset automatically.
#'
#' @param gtf_file \strong{Required.} Path to a GTF annotation file, plain
#'   text or gzip-compressed (\code{.gz}).
#' @param output_prefix \emph{Optional.} File path prefix for the four output
#'   files. For example, \code{output_prefix = "anno/gencode_hg38"} produces
#'   \code{anno/gencode_hg38_all_exons.txt.gz}, etc. The directory must
#'   already exist. Default: \code{"leafviz-annotations"}.
#'
#' @return Invisibly returns \code{output_prefix}.
#'
#' @seealso \code{\link{prepare_results}} which consumes the files produced
#'   here, \code{\link{leafviz}} to launch the Shiny visualisation.
#'
#' @examples
#' \dontrun{
#' # Generate annotation files from a GENCODE GTF, then run the full pipeline
#' gtf2leafcutter(
#'   gtf_file      = "gencode.v43.basic.annotation.gtf.gz",
#'   output_prefix = "anno/gencode_hg38"
#' )
#' prepare_results(
#'   counts_file               = "perind_numers.counts.gz",
#'   cluster_significance_file = "cluster_significance.txt",
#'   effect_sizes_file         = "effect_sizes.txt",
#'   annotation_code           = "anno/gencode_hg38"
#' )
#' leafviz("leafviz.RData")
#' }
#'
#' @export
gtf2leafcutter <- function(gtf_file, output_prefix = "leafviz-annotations") {

  stopifnot(file.exists(gtf_file))

  # ---- Read GTF ----
  message("Reading ", gtf_file)
  if (grepl("\\.gz$", gtf_file)) {
    con   <- gzfile(gtf_file, "r")
    lines <- readLines(con)
    close(con)
  } else {
    lines <- readLines(gtf_file)
  }

  # Drop blank lines and comment lines
  lines <- lines[!grepl("^\\s*$|^\\s*#", lines)]

  # Split on tab and filter to exon features (field 3)
  fields      <- strsplit(lines, "\t", fixed = TRUE)
  is_exon     <- tolower(vapply(fields, `[[`, character(1), 3)) == "exon"
  fields      <- fields[is_exon]

  if (length(fields) == 0) stop("No exon entries found in ", gtf_file)
  message(length(fields), " exon records found")

  # ---- Build data frame ----
  gtf <- data.frame(
    chr           = vapply(fields, `[[`, character(1), 1),
    start         = as.integer(vapply(fields, `[[`, character(1), 4)),
    end           = as.integer(vapply(fields, `[[`, character(1), 5)),
    strand        = vapply(fields, `[[`, character(1), 7),
    attributes    = vapply(fields, `[[`, character(1), 9),
    stringsAsFactors = FALSE
  )

  # ---- Attribute parsing ----
  # Extract the value of a single-valued GTF attribute key (vectorised over rows)
  extract_attr <- function(strings, key) {
    pattern <- paste0(key, ' "([^"]*)"')
    m       <- regexpr(pattern, strings, perl = TRUE)
    result  <- character(length(strings))
    hit     <- m > 0
    if (any(hit)) {
      # Pass full m (not m[hit]) — subsetting drops match.length attribute
      matched     <- regmatches(strings, m)
      # matched is e.g. 'gene_name "BRCA1"'; strip key + ' "' prefix and '"' suffix
      prefix_len  <- nchar(key) + 2L  # key + space + opening quote
      result[hit] <- substr(matched, prefix_len + 1L, nchar(matched) - 1L)
    }
    result
  }

  # Extract all values of a multi-valued key (e.g. "tag") and collapse with "|"
  extract_attr_multi <- function(strings, key) {
    pattern <- paste0(key, ' "([^"]*)"')
    vapply(strings, function(s) {
      m <- gregexpr(pattern, s, perl = TRUE)[[1]]
      if (m[1] == -1L) return("")
      matched <- regmatches(s, gregexpr(pattern, s, perl = TRUE))[[1]]
      vals    <- sub(paste0("^", key, ' "(.*)"$'), "\\1", matched, perl = TRUE)
      paste(vals, collapse = "|")
    }, character(1), USE.NAMES = FALSE)
  }

  message("Extracting attributes...")
  gtf$gene_name     <- extract_attr(gtf$attributes, "gene_name")
  gtf$gene_id       <- extract_attr(gtf$attributes, "gene_id")
  gtf$transcript_id <- extract_attr(gtf$attributes, "transcript_id")
  gtf$tag           <- extract_attr_multi(gtf$attributes, "tag")

  # Biotype: prefer transcript_type > gene_type > gene_biotype
  biotype           <- extract_attr(gtf$attributes, "transcript_type")
  mask              <- !nzchar(biotype)
  biotype[mask]     <- extract_attr(gtf$attributes[mask], "gene_type")
  mask              <- !nzchar(biotype)
  biotype[mask]     <- extract_attr(gtf$attributes[mask], "gene_biotype")
  biotype[!nzchar(biotype)] <- "Unknown"
  gtf$biotype       <- biotype
  gtf$attributes    <- NULL  # free memory

  # Gene name fallback: gene_id, then "Unknown"
  gtf$gene_name <- ifelse(nzchar(gtf$gene_name), gtf$gene_name,
                   ifelse(nzchar(gtf$gene_id),   gtf$gene_id, "Unknown"))

  # ---- Filter ----
  # Drop exons with no transcript_id
  n_no_tx <- sum(!nzchar(gtf$transcript_id))
  gtf     <- gtf[nzchar(gtf$transcript_id), ]

  # Within each transcript, keep only exons consistent with the first exon's
  # chr / strand / gene_name / gene_id (mirrors Perl script behaviour)
  gtf <- gtf %>%
    dplyr::group_by(transcript_id) %>%
    dplyr::filter(
      chr       == dplyr::first(chr),
      strand    == dplyr::first(strand),
      gene_name == dplyr::first(gene_name),
      gene_id   == dplyr::first(gene_id)
    ) %>%
    dplyr::ungroup()

  n_transcripts <- dplyr::n_distinct(gtf$transcript_id)
  message("Parsed ", n_transcripts, " transcripts")
  if (n_no_tx > 0) message("Skipped ", n_no_tx, " exon entries with no transcript_id")

  # ---- All exons ----
  all_exons <- gtf %>%
    dplyr::select(chr, start, end, strand, gene_name) %>%
    dplyr::distinct()

  # ---- Derive introns and splice sites ----
  # For each transcript, sort exons by start and pair consecutive exons.
  # Intron i is between exon[i].end and exon[i+1].start.
  # intron_id: 1-based rank from the 5' end of the transcript
  #   (+) strand: left-to-right numbering
  #   (-) strand: right-to-left numbering (to match Perl script)
  message("Deriving introns and splice sites...")

  intron_base <- gtf %>%
    dplyr::arrange(transcript_id, start) %>%
    dplyr::group_by(transcript_id) %>%
    dplyr::mutate(
      next_exon_start = dplyr::lead(start),
      intron_start    = end,
      intron_end      = next_exon_start,
      n_introns       = dplyr::n() - 1L,
      rank            = dplyr::row_number()
    ) %>%
    dplyr::filter(!is.na(next_exon_start), (next_exon_start - end) > 0L) %>%
    dplyr::mutate(
      intron_id = dplyr::if_else(
        strand == "+",
        rank,
        n_introns - rank + 1L
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(chr, intron_start, intron_end, gene_name, gene_id,
                  strand, transcript_id, intron_id, biotype, tag)

  # Sort order mirrors the Perl script: sort -k5,5 -k7,7 -k8,8n
  # (gene_id, transcript_id, intron_id numeric)
  bed_arrange <- function(df) dplyr::arrange(df, gene_id, transcript_id, intron_id)

  all_introns <- intron_base %>%
    dplyr::rename(start = intron_start, end = intron_end) %>%
    bed_arrange()

  all_fiveprime <- intron_base %>%
    dplyr::transmute(
      chr, start = intron_start, end = intron_start + 1L,
      gene_name, gene_id, strand, transcript_id, intron_id, biotype, tag
    ) %>%
    bed_arrange()

  all_threeprime <- intron_base %>%
    dplyr::transmute(
      chr, start = intron_end, end = intron_end + 1L,
      gene_name, gene_id, strand, transcript_id, intron_id, biotype, tag
    ) %>%
    bed_arrange()

  # ---- Write output ----
  message("Writing output files with prefix: ", output_prefix)

  write.table(
    all_exons,
    gzfile(paste0(output_prefix, "_all_exons.txt.gz")),
    sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE
  )
  write.table(
    all_introns,
    gzfile(paste0(output_prefix, "_all_introns.bed.gz")),
    sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE
  )
  write.table(
    all_fiveprime,
    gzfile(paste0(output_prefix, "_fiveprime.bed.gz")),
    sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE
  )
  write.table(
    all_threeprime,
    gzfile(paste0(output_prefix, "_threeprime.bed.gz")),
    sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE
  )

  message("Done.")
  invisible(output_prefix)
}
