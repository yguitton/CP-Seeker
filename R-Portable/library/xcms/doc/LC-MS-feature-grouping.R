## ----biocstyle, echo = FALSE, results = "asis"--------------------------------
BiocStyle::markdown()
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

## ----init, results = "hide", echo = FALSE-------------------------------------
## Silently loading all packages
library(BiocStyle)
library(xcms)
library(MsFeatures)
register(SerialParam())


## ----load-data----------------------------------------------------------------
library(xcms)
library(faahKO)
library(MsFeatures)

xmse <- loadXcmsData("xmse")

## ----fdev---------------------------------------------------------------------
featureDefinitions(xmse) |> head()

## ----filled-not-filled--------------------------------------------------------
head(featureValues(xmse, filled = FALSE))
head(featureValues(xmse, filled = TRUE))

## ----feature-rt-mz-plot, fig.width = 8, fig.height = 6, fig.cap = "Plot of retention times and m/z for all features in the data set."----
plot(featureDefinitions(xmse)$rtmed, featureDefinitions(xmse)$mzmed,
     xlab = "retention time", ylab = "m/z", main = "features",
     col = "#00000080", pch = 21, bg = "#00000040")
grid()

## -----------------------------------------------------------------------------
xmse <- groupFeatures(xmse, param = SimilarRtimeParam(10))

## -----------------------------------------------------------------------------
table(featureGroups(xmse))

## ----feature-groups-rtime-plot, fig.width = 8, fig.height = 6, fig.cap = "Feature groups defined with a rt window of 10 seconds"----
plotFeatureGroups(xmse, pch = 21, lwd = 2, col = "#00000040",
                  bg = "#00000020")
grid()

## ----repeat-------------------------------------------------------------------
## Remove previous feature grouping results to repeat the rtime-based
## feature grouping with different setting
featureDefinitions(xmse)$feature_group <- NULL

## Repeat the grouping
xmse <- groupFeatures(xmse, SimilarRtimeParam(20))
table(featureGroups(xmse))

## ----feature-groups-rtime-plot2, fig.width = 8, fig.height = 6, fig.cap = "Feature groups defined with a rt window of 20 seconds"----
plotFeatureGroups(xmse, pch = 21, lwd = 2, col = "#00000040", bg = "#00000020")
grid()

## ----abundance-correlation-heatmap, fig.cap = "Correlation of features based on feature abundances.", fig.width = 6, fig.height = 16----
library(pheatmap)
fvals <- log2(featureValues(xmse, filled = TRUE))

cormat <- cor(t(fvals), use = "pairwise.complete.obs")
ann <- data.frame(fgroup = featureGroups(xmse))
rownames(ann) <- rownames(cormat)

res <- pheatmap(cormat, annotation_row = ann, cluster_rows = TRUE,
                cluster_cols = TRUE)

## ----abundance-correlation----------------------------------------------------
xmse <- groupFeatures(
    xmse, AbundanceSimilarityParam(threshold = 0.7, transform = log2),
    filled = TRUE)
table(featureGroups(xmse))

## ----abundance-correlation-fg040, fig.width = 8, fig.height = 8, fig.cap = "Pairwise correlation plot for all features initially grouped into the feature group FG.040."----
cor_plot <- function(x, y) {
    C <- cor(x, y, use = "pairwise.complete.obs")
    col <- ifelse(C >= 0.7, yes = "#0000ff80", no = "#ff000080")
    points(x, y, pch = 16, col = col)
    grid()
}
fts <- grep("FG.040", featureGroups(xmse))
pairs(t(fvals[fts, ]), gap = 0.1, main = "FG.040", panel = cor_plot)

## ----correlate-eic, message = FALSE, warning = FALSE--------------------------
xmse <- groupFeatures(xmse, EicSimilarityParam(threshold = 0.7, n = 2))

## ----correlate-eic-result-----------------------------------------------------
table(featureGroups(xmse))

## -----------------------------------------------------------------------------
fidx <- grep("FG.013.001.", featureGroups(xmse))
eics <- featureChromatograms(
    xmse, features = rownames(featureDefinitions(xmse))[fidx],
    filled = TRUE, n = 1)

## ----example-1-eic, fig.width = 8, fig.height = 6, fig.cap = "Feature EICs per sample for features from a feature group defined by rentention time and feature abudances across samples. Features with high correlation of their EICs are shown in the same color."----
cols <- c("#ff000080", "#0000ff80")
names(cols) <- unique(featureGroups(xmse)[fidx])

plotChromatogramsOverlay(eics, col = cols[featureGroups(xmse)[fidx]],
                         lwd = 2, peakType = "none")

## ----example-1-eic-norm, fig.width = 8, fig.height = 6, fig.cap = "Feature EICs per sample normalized to an absolute intensity of 1 for features from a feature group defined by rentention time and feature abudances across samples. Features with high correlation of their EICs are shown in the same color."----
plotChromatogramsOverlay(normalize(eics),
                         col = cols[featureGroups(xmse)[fidx]],
                         lwd = 2, peakType = "none")

## -----------------------------------------------------------------------------
fidx <- grep("FG.045.001.", featureGroups(xmse))
eics <- featureChromatograms(
    xmse, features = rownames(featureDefinitions(xmse))[fidx],
    filled = TRUE, n = 1)

## ----example-2-eic, fig.width = 8, fig.height = 6, fig.cap = "Feature EICs per sample for features from a feature group defined by rentention time and feature abudances across samples. Features with high correlation of their EICs are shown in the same color."----
cols <- c("#ff000080", "#0000ff80")
names(cols) <- unique(featureGroups(xmse)[fidx])

plotChromatogramsOverlay(eics, col = cols[featureGroups(xmse)[fidx]],
                         lwd = 2, peakType = "none")

## ----example-2-eic-norm, fig.width = 8, fig.height = 6, fig.cap = "Feature EICs per sample normalized to an absolute intensity of 1 for features from a feature group defined by rentention time and feature abudances across samples. Features with high correlation of their EICs are shown in the same color."----
plotChromatogramsOverlay(normalize(eics),
                         col = cols[featureGroups(xmse)[fidx]],
                         lwd = 2, peakType = "none")

## ----reset-feature-groups-----------------------------------------------------
featureDefinitions(xmse)$feature_group <- NA_character_

set.seed(123)
fts_idx <- sample(1:nrow(featureDefinitions(xmse)), 30)
featureDefinitions(xmse)$feature_group[fts_idx] <- "FG"

## ----rtime-grouping-----------------------------------------------------------
xmse <- groupFeatures(xmse, SimilarRtimeParam(diffRt = 20))
xmse <- groupFeatures(xmse, AbundanceSimilarityParam(threshold = 0.7))
table(featureGroups(xmse))

## ----sessionInfo--------------------------------------------------------------
sessionInfo()

