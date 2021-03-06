# LeafViz 

Jack Humphrey, David A. Knowles, Yang I. Li
2017-2021


A lightweight, standalone version of the **Leafcutter Visualisation** Shiny app.

This was created due to the complexities of installing Leafcutter on local machines.

## Installation:

```
## in R:
install.packages("remotes")
remotes::install_github("jackhump/leafviz")
``` 

## Running the shiny app

### Setting the browser (for Windows only)

First set the browser option (using double backward slash or a single forward slash is due to windows) such as for the main 3 browers:
(thanks to bsouthey for this tip)


```
options(browser="C:\Program Files\Mozilla Firefox\firefox.exe")
options(browser="C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe")
options(browser="C:\Program Files\Google\Chrome\Application\chrome.exe")
```


### Running app on example dataset:

```
library(leafviz)
leafviz()
```

### Running leafviz on your own dataset:

This assumes you've prepared your differential splicing results for leafviz using the prepare_results.R script within Leafcutter.

```
library(leafviz)
leafviz("<path/to/your_leafcutter_results.RData>")
```

### Generating simple reports

Leafviz now allows you to export information to reports. Currently under development.

```
make_report(infile = "<path/to/your_leafcutter_results.RData>", outfile = "./report.html")
```

The following output is produced:

`<outfolder>/<code>_report.html` - a HTML file viewable in your browser, containing the same information as in the summary tab of the LeafViz browser session.


### Exporting tables

You can now export the clusters and introns tables to TSV with one command:

```
export_tables(infile = "<path/to/your_leafcutter_results.RData>", outfolder = ".")
```

The following outputs are produced:

`<outfolder>/<code>_sig_clusters.tsv` - the cluster-level information on each significant cluster
`<outfolder>/<code>_sig_introns.tsv` - the intron-level information for each significant cluster

`<code>` is already set in the RData object

## Classify clusters as cassette exons and determine directionality and novelty

```
classify_clusters(infile = "<path/to/your_leafcutter_results.RData>", outfolder = ".")
```


Leafviz also contains a function to classify leafcutter junction clusters as cassette exons, if they contain 3 introns with thecorrect topology (2 child (inclusion) introns; 1 parent (skipping) intron)

It then works out the directionality based on the leafcutter model effect sizes for the 3 introns. A skipped cassette exon would have a positive effect size for the parent intron and negative effect sizes for the two child introns, whereas an included exon would have the reverse.

It then uses the annotation status of each intron to determine whether the exon is novel (cryptic exon; the two inclusion introns are novel) or the skipping event is novel (skiptic exon; the skipping intron only is novel); or completely unannotated.

Exon and intron coordinates are then inferred from the junction coordinates.

The following files are produced:

`<outfolder>/<code>_classifications.tsv` - the classification for each cluster
`<outfolder>/<code>_summary.tsv` - the summary, tallying by classification

`code` is already set in the RData file.

Options:

`full_results`  - logical TRUE or FALSE - whether to also output the clusters that couldn't be classified in the output files.

