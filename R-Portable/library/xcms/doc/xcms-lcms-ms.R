## ----biocstyle, echo = FALSE, results = "asis"--------------------------------
BiocStyle::markdown()

## ----init, message = FALSE, echo = FALSE, results = "hide"--------------------
## Silently loading all packages
library(BiocStyle)
library(xcms)
library(pander)
register(SerialParam())


## ----load-dda-data, message = FALSE-------------------------------------------
library(xcms)

dda_file <- system.file("TripleTOF-SWATH/PestMix1_DDA.mzML",
                        package = "msdata")
dda_data <- readMSData(dda_file, mode = "onDisk")

## ----subset-dda, echo = FALSE, message = FALSE, eval = TRUE-------------------
#' Silently sub-setting the object to speed-up analysis
dda_data <- filterRt(dda_data, rt = c(200, 600))

## ----dda-table-mslevel--------------------------------------------------------
table(msLevel(dda_data))

## ----dda-find-chrom-peaks-ms1, message = FALSE--------------------------------
cwp <- CentWaveParam(snthresh = 5, noise = 100, ppm = 10,
                     peakwidth = c(3, 30))
dda_data <- findChromPeaks(dda_data, param = cwp)

## ----dda-spectra, message = FALSE---------------------------------------------
dda_spectra <- chromPeakSpectra(dda_data)
dda_spectra

## ----dda-ms2-example, message = FALSE-----------------------------------------
ex_mz <- 304.1131
chromPeaks(dda_data, mz = ex_mz, ppm = 20)

## ----dda-ms2-get-ms2, message = FALSE-----------------------------------------
ex_id <- rownames(chromPeaks(dda_data, mz = ex_mz, ppm = 20))
ex_spectra <- dda_spectra[mcols(dda_spectra)$peak_id == ex_id]
ex_spectra

## ----dda-ms2-consensus--------------------------------------------------------
ex_spectrum <- combineSpectra(ex_spectra, method = consensusSpectrum, mzd = 0,
                              ppm = 20, minProp = 0.8, weighted = FALSE,
                              intensityFun = median, mzFun = median)
ex_spectrum

## ----dda-ms2-consensus-plot, message = FALSE, fig.cap = "Consensus MS2 spectrum created from all measured MS2 spectra for ions of chromatographic peak CP53.", fig.width = 8, fig.height = 8----
plot(ex_spectrum[[1]])

## ----dda-ms2-metlin-match, fig.cap = "Mirror plots for the candidate MS2 spectrum against Flumanezil (left) and Fenamiphos (right). The upper panel represents the candidate MS2 spectrum, the lower the target MS2 spectrum. Matching peaks are indicated with a dot.", fig.width = 12, fig.height = 6----
flumanezil <- spectra(readMgfData(
    system.file("mgf/metlin-2724.mgf", package = "xcms")))
fenamiphos <- spectra(readMgfData(
    system.file("mgf/metlin-72445.mgf", package = "xcms")))

par(mfrow = c(1, 2))
plot(ex_spectrum[[1]], flumanezil[[3]], main = "against Flumanezil",
     tolerance = 40e-6)
plot(ex_spectrum[[1]], fenamiphos[[3]], main = "against Fenamiphos",
     tolerance = 40e-6)

## ----dda-ms2-dotproduct-------------------------------------------------------
compareSpectra(ex_spectrum[[1]], flumanezil[[3]], binSize = 0.02,
               fun = "dotproduct")
compareSpectra(ex_spectrum[[1]], fenamiphos[[3]], binSize = 0.02,
               fun = "dotproduct")

## ----load-swath-data, message = FALSE-----------------------------------------
swath_file <- system.file("TripleTOF-SWATH",
                          "PestMix1_SWATH.mzML",
                          package = "msdata")

swath_data <- readMSData(swath_file, mode = "onDisk")

## ----echo = FALSE, message = FALSE, eval = TRUE-------------------------------
swath_data <- filterRt(swath_data, rt = c(200, 600))

## ----swath-table-mslevel------------------------------------------------------
table(msLevel(swath_data))

## ----fdata-isolationwindow----------------------------------------------------
head(fData(swath_data)[, c("isolationWindowTargetMZ",
                           "isolationWindowLowerOffset",
                           "isolationWindowUpperOffset",
                           "msLevel", "retentionTime")])

head(isolationWindowLowerMz(swath_data))
head(isolationWindowUpperMz(swath_data))

## -----------------------------------------------------------------------------
table(isolationWindowTargetMz(swath_data))

## ----find-chrom-peaks-ms1, message = FALSE------------------------------------
cwp <- CentWaveParam(snthresh = 5, noise = 100, ppm = 10,
                     peakwidth = c(3, 30))
swath_data <- findChromPeaks(swath_data, param = cwp)

## ----find-chrom-peaks-ms2, message = FALSE------------------------------------
cwp <- CentWaveParam(snthresh = 3, noise = 10, ppm = 10,
                     peakwidth = c(3, 30))
swath_data <- findChromPeaksIsolationWindow(swath_data, param = cwp)

## -----------------------------------------------------------------------------
chromPeakData(swath_data)

## -----------------------------------------------------------------------------
table(chromPeakData(swath_data)$isolationWindow)

## ----fena-extract-peak--------------------------------------------------------
fenamiphos_mz <- 304.113077
fenamiphos_ms1_peak <- chromPeaks(swath_data, mz = fenamiphos_mz, ppm = 2)
fenamiphos_ms1_peak

## ----fena-identify-ms2--------------------------------------------------------
keep <- chromPeakData(swath_data)$isolationWindowLowerMz < fenamiphos_mz &
        chromPeakData(swath_data)$isolationWindowUpperMz > fenamiphos_mz

## ----fena-check-rt------------------------------------------------------------
keep <- keep &
    chromPeaks(swath_data)[, "rtmin"] < fenamiphos_ms1_peak[, "rt"] &
    chromPeaks(swath_data)[, "rtmax"] > fenamiphos_ms1_peak[, "rt"]

fenamiphos_ms2_peak <- chromPeaks(swath_data)[which(keep), ]

## ----fena-eic-extract, warning = FALSE----------------------------------------
rtr <- fenamiphos_ms1_peak[, c("rtmin", "rtmax")]
rtr[1] <- rtr[1] - 1
rtr[2] <- rtr[2] + 1
mzr <- fenamiphos_ms1_peak[, c("mzmin", "mzmax")]
fenamiphos_ms1_chr <- chromatogram(swath_data, rt = rtr, mz = mzr)

rtr <- fenamiphos_ms2_peak[, c("rtmin", "rtmax")]
rtr[, 1] <- rtr[, 1] - 1
rtr[, 2] <- rtr[, 2] + 1
mzr <- fenamiphos_ms2_peak[, c("mzmin", "mzmax")]
fenamiphos_ms2_chr <- chromatogram(
    filterIsolationWindow(swath_data, mz = fenamiphos_mz),
    rt = rtr, mz = mzr, msLevel = 2L)

## ----fena-eic-plot, fig.width = 10, fig.height = 5, fig.cap = "Extracted ion chromatograms for Fenamiphos from MS1 (blue) and potentially related signal in MS2 (grey)."----
plot(rtime(fenamiphos_ms1_chr[1, 1]),
     intensity(fenamiphos_ms1_chr[1, 1]),
     xlab = "retention time [s]", ylab = "intensity", pch = 16,
     ylim = c(0, 5000), col = "blue", type = "b", lwd = 2)
#' Add data from all MS2 peaks
tmp <- lapply(fenamiphos_ms2_chr@.Data,
              function(z) points(rtime(z), intensity(z),
                                 col = "#00000080",
                                 type = "b", pch = 16))

## ----fena-cor-----------------------------------------------------------------
correlate(fenamiphos_ms2_chr[1, 1],
          fenamiphos_ms1_chr[1, 1], align = "approx")

## ----reconstruct-ms2, message = FALSE-----------------------------------------
swath_spectra <- reconstructChromPeakSpectra(swath_data, minCor = 0.9)
swath_spectra

## ----fena-swath-peak----------------------------------------------------------
fenamiphos_swath_spectrum <- swath_spectra[
    mcols(swath_spectra)$peak_id == rownames(fenamiphos_ms1_peak)]

## ----fena-swath-plot, fig.cap = "Mirror plot comparing the reconstructed MS2 spectrum for Fenamiphos (upper panel) against the measured spectrum from the DDA data and the Fenamiphhos spectrum from Metlin.", fig.width = 12, fig.height = 6----
par(mfrow = c(1, 2))
plot(fenamiphos_swath_spectrum[[1]], ex_spectrum[[1]],
     tolerance = 50e-6, main = "against DDA")
plot(fenamiphos_swath_spectrum[[1]], fenamiphos[[2]],
     tolerance = 50e-6, main = "against Metlin")

## ----pro-swath----------------------------------------------------------------
prochloraz_mz <- 376.0381

prochloraz_ms1_peak <- chromPeaks(swath_data, msLevel = 1L,
                                  mz = prochloraz_mz, ppm = 5)
prochloraz_ms1_peak

prochloraz_swath_spectrum <- swath_spectra[
    mcols(swath_spectra)$peak_id == rownames(prochloraz_ms1_peak)]

## ----pro-dda------------------------------------------------------------------
prochloraz_dda_peak <- chromPeaks(dda_data, msLevel = 1L,
                                  mz = prochloraz_mz, ppm = 5)
prochloraz_dda_peak

## ----pro-dda-ms2--------------------------------------------------------------
prochloraz_dda_spectra <- dda_spectra[
    mcols(dda_spectra)$peak_id == rownames(prochloraz_dda_peak)]
prochloraz_dda_spectra

## ----pro-dda-consensus--------------------------------------------------------
prochloraz_dda_spectrum <- combineSpectra(
    prochloraz_dda_spectra, method = consensusSpectrum, msd = 0, ppm = 20,
    minProp = 0.8, weighted = FALSE, intensityFun = median, mzFun = median)

## ----prochloraz-metlin--------------------------------------------------------
prochloraz <- spectra(readMgfData(
    system.file("mgf/metlin-68898.mgf", package = "xcms")))

## ----pro-swath-plot, fig.cap = "Mirror plot comparing the reconstructed MS2 spectrum for Prochloraz (upper panel) against the measured spectrum from the DDA data and the Prochloraz spectrum from Metlin.", fig.width = 12, fig.height = 6----
par(mfrow = c(1, 2))
plot(prochloraz_swath_spectrum[[1]], prochloraz_dda_spectrum[[1]],
     tolerance = 50e-6, main = "against DDA")
plot(prochloraz_swath_spectrum[[1]], prochloraz[[2]],
     tolerance = 50e-6, main = "against Metlin")

## ----sessionInfo--------------------------------------------------------------
sessionInfo()

