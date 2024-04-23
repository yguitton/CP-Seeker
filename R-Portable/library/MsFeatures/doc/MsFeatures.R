## ----style, echo = FALSE, results = 'asis'------------------------------------
BiocStyle::markdown()

## ----echo = FALSE, warning = FALSE, message = FALSE---------------------------
library(MsFeatures)
library(SummarizedExperiment)

## ----load-data, message = FALSE-----------------------------------------------
library(MsFeatures)
library(SummarizedExperiment)

data("se")

## ----fdev---------------------------------------------------------------------
rowData(se)
head(assay(se))

## ----feature-rt-mz-plot, fig.width = 8, fig.height = 6, fig.cap = "Plot of retention times and m/z for all features in the data set."----
plot(rowData(se)$rtmed, rowData(se)$mzmed,
     xlab = "retention time", ylab = "m/z", main = "features",
     col = "#00000060")
grid()

## -----------------------------------------------------------------------------
se <- groupFeatures(se, param = SimilarRtimeParam(10), rtime = "rtmed")

## -----------------------------------------------------------------------------
table(featureGroups(se))

## -----------------------------------------------------------------------------
split(rowData(se)$rtmed, featureGroups(se)) |>
vapply(FUN = mean, numeric(1)) |>
sort()

## ----abundance-correlation-heatmap, fig.cap = "Correlation of features based on their abundances.", fig.width = 12, fig.height = 14----
library(pheatmap)
fvals <- log2(assay(se))

cormat <- cor(t(fvals), use = "pairwise.complete.obs")
ann <- data.frame(fgroup = featureGroups(se))
rownames(ann) <- rownames(cormat)

res <- pheatmap(cormat, annotation_row = ann, cluster_rows = TRUE,
                cluster_cols = TRUE)

## ----abundance-correlation----------------------------------------------------
se <- groupFeatures(se, AbundanceSimilarityParam(threshold = 0.7,
                                                 transform = log2), i = 1)
table(featureGroups(se))

## ----abundance-correlation-fg003, fig.width = 8, fig.height = 8, fig.cap = "Pairwise correlation plot for  features initially grouped into the feature group FG.003."----
fts <- grep("FG.003", featureGroups(se))
pairs(t(fvals[fts, ]), gap = 0.1, main = "FG.003")

## ----abundance-correlation-fg008, fig.width = 8, fig.height = 8, fig.cap = "Pairwise correlation plot for  features initially grouped into the feature group FG.008."----
fts <- grep("FG.008", featureGroups(se))
pairs(t(fvals[fts, ]), gap = 0.1, main = "FG.008")

## ----abundance-correlation-fg008-table, results = "asis"----------------------
tmp <- as.data.frame(rowData(se)[fts, c("rtmed", "mzmed", "feature_group")])
tmp <- tmp[order(tmp$feature_group), ]
knitr::kable(tmp)

## -----------------------------------------------------------------------------
featureGroups(se) <- NA_character_
featureGroups(se)[30:60] <- "FG"

se <- groupFeatures(se, SimilarRtimeParam(10), rtime = "rtmed")

## -----------------------------------------------------------------------------
featureGroups(se)

## ----sessioninfo, echo=FALSE--------------------------------------------------
sessionInfo()

