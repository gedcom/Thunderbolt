is.installed = function(packages) sapply(packages, function(p) is.element(p, installed.packages()[,1]))

# CRAN packages
cran.packages = c(
	"tidyverse",
	"readxl",	# Excel support
	"writexl",
	"networkD3",	# networkD3 package for rendering network graphs
	"ggplot2",	# Plots
	"ggdendro",	# Draw dendrograms with ggplot2
	"shinyBS",	# Make pop-ups
	"igraph",	# Facilitates network graphs
	"devtools",	# Required to download packages from Github (see below)
	"gplots",    	# gplots graphical
	"shinyjs",	# Easy javascript for Shiny
	"plotly",	# Yet another interactive plotting tool
	"VennDiagram", # Venn diagrams
	"shinycssloaders",
	"magrittr"
)

# Bioconductor packages
biocon.packages = c(
    "limma",
    "impute",
    "AnnotationDbi",
    "org.Hs.eg.db",
    "org.Mm.eg.db",
    "org.Rn.eg.db",
    "org.Dm.eg.db",
    "RBGL",
    "graph",
    "biomaRt",
    "annotationTools",
    "sva"
)

# Github packages
github.packages = c(
    "rstudio/DT",
    "rstudio/d3heatmap",
    "ramnathv/rCharts",
    "js229/Vennerable"
)

installed = sapply(cran.packages, is.installed)
if (!all(installed))
{
	cat("Installing CRAN packages...\n")
	install.packages(cran.packages[!installed])
} else cat("No CRAN packages to install.\n")

if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

installed = sapply(biocon.packages, is.installed)
if (!all(installed))
{
	cat("Installing Bioconductor packages...\n")
	BiocManager::install(biocon.packages[!installed])	
} else cat("No Bioconductor packages to install.\n")

# Github
installed = sapply(github.packages, is.installed)
if (!all(installed))
{
	cat("Installing GitHub packages...\n")
	devtools::install_github(github.packages[!installed])
} else cat("No GitHub packages to install.\n")
