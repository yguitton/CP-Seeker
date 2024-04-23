## ----style, echo = FALSE, results = 'asis'------------------------------------
BiocStyle::markdown()

## ----env, message = FALSE, warning = FALSE, echo = FALSE----------------------
library("QFeatures")
library("ggplot2")
library("dplyr")

## ----msdata-------------------------------------------------------------------
basename(f <- msdata::quant(pattern = "cptac", full.names = TRUE))

## ----cptac_cols---------------------------------------------------------------
names(read.delim(f))
(i <- grep("Intensity\\.", names(read.delim(f))))

## ----read_cptac---------------------------------------------------------------
library("QFeatures")
cptac <- readQFeatures(f, ecol = i, sep = "\t", name = "peptides", fnames = "Sequence")

## -----------------------------------------------------------------------------
cptac$group <- rep(c("6A", "6B"), each = 3)
cptac$sample <- rep(7:9, 2)
colData(cptac)

## -----------------------------------------------------------------------------
filterFeatures(cptac, ~ Reverse == "")

## -----------------------------------------------------------------------------
filterFeatures(cptac, ~ Potential.contaminant == "")

## -----------------------------------------------------------------------------
cptac <- cptac |>
    filterFeatures(~ Reverse == "") |>
    filterFeatures(~ Potential.contaminant == "")

## -----------------------------------------------------------------------------
rowDataNames(cptac)

## -----------------------------------------------------------------------------
rowvars <- c("Sequence", "Proteins", "Leading.razor.protein")
cptac <- selectRowData(cptac, rowvars)
rowDataNames(cptac)

## -----------------------------------------------------------------------------
cptac <- zeroIsNA(cptac, i = seq_along(cptac))
nNA(cptac, i = seq_along(cptac))

## -----------------------------------------------------------------------------
cptac <- filterNA(cptac, i = seq_along(cptac), pNA = 0)
cptac

## ----count_peptides-----------------------------------------------------------
cptac <- countUniqueFeatures(cptac,
                             i = "peptides",
                             colDataName = "peptide_counts")
colData(cptac)

## ----count_proteins-----------------------------------------------------------
cptac <- countUniqueFeatures(cptac,
                             i = "peptides",
                             groupBy = "Proteins",
                             colDataName = "protein_counts")
colData(cptac)

## ----miximp, echo = FALSE, fig.cap = "Mixed imputation method. Black cells represent presence of quantitation values and light grey corresponds to missing data. The two groups of interest are depicted in green and blue along the heatmap columns. Two classes of proteins are annotated on the left: yellow are proteins with randomly occurring missing values (if any) while proteins in brown are candidates for non-random missing value imputation."----
data(se_na2)
x <- assay(impute(se_na2, "zero"))
x[x != 0] <- 1
suppressPackageStartupMessages(library("gplots"))
heatmap.2(x, col = c("lightgray", "black"),
          scale = "none", dendrogram = "none",
          trace = "none", keysize = 0.5, key = FALSE,
          RowSideColors = ifelse(rowData(se_na2)$randna, "orange", "brown"),
          ColSideColors = rep(c("steelblue", "darkolivegreen"), each = 8))

## -----------------------------------------------------------------------------
cptac <- addAssay(cptac,
                  logTransform(cptac[[1]]),
                  name = "peptides_log")
cptac

## ----eval = FALSE-------------------------------------------------------------
#  logTransform(cptac,
#               i = "peptides",
#               name = "log_peptides")

## ----fig.cap = "Quantitative data in its original scale (left) and log2-transformed (right)."----
par(mfrow = c(1, 2))
limma::plotDensities(assay(cptac[[1]]))
limma::plotDensities(assay(cptac[[2]]))

## -----------------------------------------------------------------------------
cptac <- addAssay(cptac,
                  normalize(cptac[["peptides_log"]], method = "center.median"),
                  name = "peptides_norm")
cptac

## ----eval = FALSE-------------------------------------------------------------
#  normalize(cptac,
#            i = "log_peptides",
#            name = "lognorm_peptides",
#            method = "center.median")

## ----fig.cap = "Distribution of log2 peptide intensities before (left) and after (right) median normalisation."----
par(mfrow = c(1, 2))
limma::plotDensities(assay(cptac[["peptides_log"]]))
limma::plotDensities(assay(cptac[["peptides_norm"]]))

## ----warning = FALSE----------------------------------------------------------
cptac <- aggregateFeatures(cptac, i = "peptides_norm", fcol = "Proteins", name = "proteins")
cptac

## -----------------------------------------------------------------------------
head(assay(cptac[["proteins"]]))
rowData(cptac[["proteins"]])

## -----------------------------------------------------------------------------
table(rowData(cptac[["proteins"]])$.n)

## ----message = TRUE, fig.cap = "Expression intensities for the protein *P02787ups|TRFE_HUMAN_UPS* (right, green) and its peptides (left) in groups A (circles) and B (triangles)."----
library("ggplot2")
library("dplyr")
longFormat(cptac["P02787ups|TRFE_HUMAN_UPS", ]) |>
    as.data.frame() |>
    mutate(group = ifelse(grepl("A", colname), "A", "B")) |>
    mutate(sample = sub("Intensity\\.", "", colname)) |>
    ggplot(aes(x = sample, y = value, colour = rowname, shape = group)) +
    geom_point() +
    facet_grid(~ assay)

## ----sessioninfo, echo=FALSE--------------------------------------------------
sessionInfo()

