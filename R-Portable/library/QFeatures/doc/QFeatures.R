## ----style, echo = FALSE, results = 'asis'------------------------------------
BiocStyle::markdown()

## ----env, message = FALSE, warning = FALSE, echo = FALSE----------------------
library("QFeatures")

## ----featuresplot, fig.cap = "Conceptual representation of a `QFeatures` object and the aggregative relation between different assays.", echo = FALSE----
par(mar = c(0, 0, 0, 0))
plot(NA, xlim = c(0, 12), ylim = c(0, 20),
     xaxt = "n", yaxt = "n",
     xlab = "", ylab = "", bty = "n")

for (i in 0:7)
    rect(0, i, 3, i+1, col = "lightgrey", border = "white")


for (i in 8:12)
    rect(0, i, 3, i+1, col = "steelblue", border = "white")

for (i in 13:18)
    rect(0, i, 3, i+1, col = "orange", border = "white")

for (i in 19)
    rect(0, i, 3, i+1, col = "darkgrey", border = "white")


for (i in 5:7)
    rect(5, i, 8, i+1, col = "lightgrey", border = "white")

for (i in 8:10)
    rect(5, i, 8, i+1, col = "steelblue", border = "white")

for (i in 11:13)
    rect(5, i, 8, i+1, col = "orange", border = "white")

for (i in 14)
    rect(5, i, 8, i+1, col = "darkgrey", border = "white")

rect(9, 8, 12, 8+1, col = "lightgrey", border = "white")
rect(9, 9, 12, 9+1, col = "steelblue", border = "white")
rect(9, 10, 12, 10+1, col = "orange", border = "white")
rect(9, 11, 12, 11+1, col = "darkgrey", border = "white")

segments(3, 8, 5, 8, lty = "dashed")
segments(3, 6, 5, 7, lty = "dashed")
segments(3, 4, 5, 6, lty = "dashed")
segments(3, 0, 5, 5, lty = "dashed")

segments(3, 10, 5, 9, lty = "dashed")
segments(3, 11, 5, 10, lty = "dashed")
segments(3, 13, 5, 11, lty = "dashed")

segments(3, 14, 5, 12, lty = "dashed")
segments(3, 16, 5, 13, lty = "dashed")
segments(3, 19, 5, 14, lty = "dashed")

segments(3, 20, 5, 15, lty = "dashed")


segments(8, 5, 9, 8, lty = "dashed")
segments(8, 8, 9, 9, lty = "dashed")
segments(8, 11, 9, 10, lty = "dashed")
segments(8, 14, 9, 11, lty = "dashed")
segments(8, 15, 9, 12, lty = "dashed")

## ----loadpkg------------------------------------------------------------------
library("QFeatures")

## ----loaddfr, echo = FALSE----------------------------------------------------
data(hlpsms)

## ----readQFeatures------------------------------------------------------------
data(hlpsms)
hl <- readQFeatures(hlpsms, ecol = 1:10, name = "psms")
hl

## ----subsetassay--------------------------------------------------------------
hl[[1]]
hl[["psms"]]
head(assay(hl[["psms"]]))
head(rowData(hl[["psms"]]))

## ----aggregateFeatures1-------------------------------------------------------
hl <- aggregateFeatures(hl, "psms", "Sequence", name = "peptides", fun = colMeans)
hl
hl[["peptides"]]

## ----aggregateFeatures2-------------------------------------------------------
hl <- aggregateFeatures(hl, "peptides", "ProteinGroupAccessions", name = "proteins", fun = colMeans)
hl
hl[["proteins"]]

## -----------------------------------------------------------------------------
colData(hl)
hl$tag <- c("126", "127N", "127C", "128N", "128C", "129N", "129C",
            "130N", "130C", "131")
colData(hl)

## ----rowDataNames-------------------------------------------------------------
rowDataNames(hl)

## ----rowData------------------------------------------------------------------
rowData(hl)

## ----rbindRowData-------------------------------------------------------------
rbindRowData(hl, i = c("peptides", "proteins"))

## -----------------------------------------------------------------------------
dF <- DataFrame(mean = rowSums(assay(hl[["proteins"]])),
                sd = rowSds(assay(hl[["proteins"]])))

## -----------------------------------------------------------------------------
rowData(hl) <- List(proteins = dF)

## -----------------------------------------------------------------------------
rowData(hl)[["proteins"]]

## ----stat3--------------------------------------------------------------------
stat3 <- hl["P42227-2", , ]
stat3

## ----plotstat3----------------------------------------------------------------
stat3_df <- data.frame(longFormat(stat3))
stat3_df$assay <- factor(stat3_df$assay,
                        levels = c("psms", "peptides", "proteins"))
library("ggplot2")
ggplot(data = stat3_df,
       aes(x = colname,
           y = value,
           group = rowname)) +
    geom_line() + geom_point() +
    facet_grid(~ assay)

## ----stat---------------------------------------------------------------------
stat <- hl[c("P42227-2", "P42225"), , ]
stat

## ----plotstat-----------------------------------------------------------------
stat_df <- data.frame(longFormat(stat))
stat_df$stat3 <- ifelse(stat_df$rowname %in% stat3_df$rowname,
                        "STAT3", "STAT1")
stat_df$assay <- factor(stat_df$assay,
                        levels = c("psms", "peptides", "proteins"))

ggplot(data = stat_df,
       aes(x = colname,
           y = value,
           group = rowname)) +
    geom_line() + geom_point() +
    facet_grid(stat3 ~ assay)

## ----subsetByFeature----------------------------------------------------------
hl |>
    subsetByFeature("P42227-2")

hl |>
    subsetByFeature(c("P42227-2", "P42225"))

## ----subsetpipe, eval = FALSE-------------------------------------------------
#  hl |>
#      subsetByFeature("P42227-2") |>
#      longFormat() |>
#      as.data.frame |>
#      ggplot(aes(x = colname,
#                 y = value,
#                 group = rowname)) +
#      geom_line() +
#      facet_grid(~ assay)

## ----varfilter----------------------------------------------------------------
mito_filter <- VariableFilter(field = "markers",
                              value = "Mitochondrion",
                              condition = "==")
mito_filter

qval_filter <- VariableFilter(field = "qValue",
                              value = 0.001,
                              condition = "<=")
qval_filter

## ----mito_filter--------------------------------------------------------------
filterFeatures(hl, mito_filter)

## ----qval_filter--------------------------------------------------------------
filterFeatures(hl, qval_filter)

## ----formula_filter-----------------------------------------------------------
filterFeatures(hl, ~ markers == "Mitochondrion")
filterFeatures(hl, ~ qValue <= 0.001)

## ----sessioninfo, echo=FALSE--------------------------------------------------
sessionInfo()

