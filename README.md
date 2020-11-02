# LeafViz 

Jack Humphrey, David A. Knowles, Yang I. Li
2017-2020


A lightweight, standalone version of the **Leafcutter Visualisation** Shiny app.

This was created due to the complexities of installing Leafcutter on local machines.

## Installation:

```
## in R:
install.packages("remotes")
remotes::install_github("jackhump/leafviz")
``` 

## Running app on example dataset:

```
in R:
library(leafviz)
leafviz()
```

## Running leafviz on your own dataset:

This assumes you've prepared your differential splicing results for leafviz using the prepare_results.R script within Leafcutter.

```
library(leafviz)
leafviz("<path/to/your_leafcutter_results.RData>")
```
