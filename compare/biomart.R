#source("https://bioconductor.org/biocLite.R")
#biocLite("AnnotationDbi")
#biocLite("biomaRt")
#biocLite("annotationTools")
library(biomaRt)
library(annotationTools)

# Read the human housekeeping genes
human.genes.file <- "Eisenberg2003_Human_Housekeeping_Genes.txt"
human.genes <- readLines(human.genes.file) # 553 genes
# There are some duplicate genes so remove these
human.genes <- unique(human.genes) # 549 unique genes

# Map the human gene symbols to Entrez ids using BioMart.
# Note that this produces the exact same set of Entrez id's as AnnotationDbi.
human.ensembl <- useMart("ensembl", dataset="hsapiens_gene_ensembl")
human.entrez <- getBM(attributes=c("hgnc_symbol", "entrezgene"), filters="hgnc_symbol", values=human.genes, mart=human.ensembl)

# Using the human Entrez ids, map these to mouse Entrez ids and MGI symbols using BioMart
mouse.ensembl <- useMart("ensembl", dataset="mmusculus_gene_ensembl")
mapped.biomart <- getLDS(attributes=c("entrezgene", "hgnc_symbol"), filters="entrezgene", values=human.entrez$entrezgene, mart=human.ensembl, attributesL=c("entrezgene", "mgi_symbol"), martL=mouse.ensembl, uniqueRows=TRUE)
mapped.biomart <- mapped.biomart[order(mapped.biomart$HGNC.symbol),]

# Again map human Entrez ids to mouse Entrez ids and MGI symbols using Homologene
homologene <- read.delim("homologene.data", header=FALSE)
mapped.homolog <- unlist(getHOMOLOG(human.entrez$entrezgene, 10090, homologene, cluster=FALSE, diagnose=FALSE, noIDsymbol=NA, clusterCol=1, speciesCol=2, idCol=3))
mapped.homolog.symbols <- getBM(attributes=c("entrezgene", "mgi_symbol"), filters="entrezgene", values=as.character(mapped.homolog), mart=mouse.ensembl) # 509 genes
# Some entrez ids (12846, 18739, 19367, 22273, 58988) have been mapped to blank gene symbols but each of these has another entry with the correct gene symbol. Remove the blank genes.
mapped.homolog.symbols <- mapped.homolog.symbols[!mapped.homolog.symbols$mgi_symbol=="",] # 504 genes

# Produce a Venn diagram. There are 87 genes from Biomart only, 16 genes from Homologene only, and 482 in the intersection.
venn(list(MouseHomolog=unique(mapped.homolog.symbols$mgi_symbol), MouseBiomart=unique(mapped.biomart$MGI.symbol)))

# Combine the mouse symbols obtained from BioMart and Homologene
mouse.genes <- union(mapped.homolog.symbols$mgi_symbol, mapped.biomart$MGI.symbol)
# Sort genes alphabetically 
mouse.genes <- sort(mouse.genes) # 585 genes