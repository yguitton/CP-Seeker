## ----biocstyle, echo = FALSE, results = "asis"--------------------------------
BiocStyle::markdown()

## ----init, message = FALSE, echo = FALSE, results = "hide"--------------------
## Silently loading all packages
library(BiocStyle)
library(xcms)
library(Spectra)
library(pander)
register(SerialParam())


## ----load-dda-data, message = FALSE-------------------------------------------
library(xcms)
library(MsExperiment)

dda_file <- system.file("TripleTOF-SWATH", "PestMix1_DDA.mzML",
                        package = "msdata")
dda_data <- readMsExperiment(dda_file)
chr <- chromatogram(dda_data, aggregationFun = "sum", msLevel = 1L)

## ----fig.caption = "Total ion chromatogram (MS1) of the DDA data."------------
plot(chr)
abline(v = c(230, 610))
## filter the data
dda_data <- filterRt(dda_data, rt = c(230, 610))

## ----dda-table-mslevel--------------------------------------------------------
dda_data[1] |>
spectra() |>
msLevel() |>
table()

## ----precursor----------------------------------------------------------------
dda_data[1] |>
spectra() |>
filterMsLevel(2) |>
precursorMz() |>
head()

## ----precursor-intensity------------------------------------------------------
dda_data[1] |>
spectra() |>
filterMsLevel(2) |>
precursorIntensity() |>
head()

## ----estimate-precursor-------------------------------------------------------
prec_int <- estimatePrecursorIntensity(spectra(dda_data))

## ----set-precursor-intensity--------------------------------------------------
spectra(dda_data)$precursorIntensity <- prec_int

dda_data[1] |>
spectra() |>
filterMsLevel(2) |>
precursorIntensity() |>
head()

## ----dda-find-chrom-peaks-ms1, message = FALSE--------------------------------
cwp <- CentWaveParam(snthresh = 5, noise = 100, ppm = 10,
                     peakwidth = c(3, 30))
dda_data <- findChromPeaks(dda_data, param = cwp, msLevel = 1L)

## ----dda-spectra, message = FALSE---------------------------------------------
library(Spectra)
dda_spectra <- chromPeakSpectra(dda_data, msLevel = 2L)
dda_spectra

## ----peak_id------------------------------------------------------------------
dda_spectra$peak_id

## ----dda-ms2-example, message = FALSE-----------------------------------------
ex_mz <- 304.1131
chromPeaks(dda_data, mz = ex_mz, ppm = 20)

## ----dda-ms2-get-ms2, message = FALSE-----------------------------------------
ex_id <- rownames(chromPeaks(dda_data, mz = ex_mz, ppm = 20))
ex_spectra <- dda_spectra[dda_spectra$peak_id == ex_id]
ex_spectra

## ----dda-ms2-consensus, message = FALSE---------------------------------------
ex_spectrum <- combineSpectra(ex_spectra, FUN = combinePeaks, ppm = 20,
                              peaks = "intersect", minProp = 0.8,
                              intensityFun = median, mzFun = median,
                              f = ex_spectra$peak_id)
ex_spectrum

## ----dda-ms2-consensus-plot, message = FALSE, fig.cap = "Consensus MS2 spectrum created from all measured MS2 spectra for ions of chromatographic peak CP53.", fig.width = 8, fig.height = 8----
plotSpectra(ex_spectrum)

## ----normalize----------------------------------------------------------------
scale_fun <- function(z, ...) {
    z[, "intensity"] <- z[, "intensity"] /
        max(z[, "intensity"], na.rm = TRUE) * 100
    z
}
ex_spectrum <- addProcessing(ex_spectrum, FUN = scale_fun)

## ----dda-ms2-metlin-match, fig.cap = "Mirror plots for the candidate MS2 spectrum against Flumanezil (left) and Fenamiphos (right). The upper panel represents the candidate MS2 spectrum, the lower the target MS2 spectrum. Matching peaks are indicated with a dot.", fig.width = 12, fig.height = 6----
library(MsBackendMgf)
flumanezil <- Spectra(
    system.file("mgf", "metlin-2724.mgf", package = "xcms"),
    source = MsBackendMgf())
fenamiphos <- Spectra(
    system.file("mgf", "metlin-72445.mgf", package = "xcms"),
    source = MsBackendMgf())

par(mfrow = c(1, 2))
plotSpectraMirror(ex_spectrum, flumanezil[3], main = "against Flumanezil",
                  ppm = 40)
plotSpectraMirror(ex_spectrum, fenamiphos[3], main = "against Fenamiphos",
                  ppm = 40)

## ----dda-ms2-dotproduct-------------------------------------------------------
compareSpectra(ex_spectrum, flumanezil, ppm = 40)
compareSpectra(ex_spectrum, fenamiphos, ppm = 40)

## ----load-swath-data, message = FALSE-----------------------------------------
swath_file <- system.file("TripleTOF-SWATH",
                          "PestMix1_SWATH.mzML",
                          package = "msdata")

swath_data <- readMsExperiment(swath_file)
swath_data <- filterRt(swath_data, rt = c(230, 610))


## ----swath-table-mslevel------------------------------------------------------
spectra(swath_data) |>
msLevel() |>
table()

## ----fdata-isolationwindow----------------------------------------------------
spectra(swath_data) |>
spectraData(c("isolationWindowTargetMz", "isolationWindowLowerMz",
              "isolationWindowUpperMz", "msLevel", "rtime")) |>
head()


## -----------------------------------------------------------------------------
head(isolationWindowLowerMz(spectra(swath_data)))
head(isolationWindowUpperMz(spectra(swath_data)))

## -----------------------------------------------------------------------------
table(isolationWindowTargetMz(spectra(swath_data)))

## ----fig.cap = "TIC for MS1 (upper panel) and MS2 data from the isolation window with target m/z 270.85 (lower panel)."----
tic_ms1 <- chromatogram(swath_data, msLevel = 1L, aggregationFun = "sum")
tic_ms2 <- chromatogram(swath_data, msLevel = 2L, aggregationFun = "sum",
                        isolationWindowTargetMz = 270.85)
par(mfrow = c(2, 1))
plot(tic_ms1, main = "MS1")
plot(tic_ms2, main = "MS2, isolation window m/z 270.85")

## ----eval = FALSE-------------------------------------------------------------
#  spectra(swath_data)$isolationWindowTargetMz <- precursorMz(spectra(swath_data))

## ----find-chrom-peaks-ms1, message = FALSE------------------------------------
cwp <- CentWaveParam(snthresh = 5, noise = 100, ppm = 10,
                     peakwidth = c(3, 30))
swath_data <- findChromPeaks(swath_data, param = cwp)
swath_data

## ----find-chrom-peaks-ms2, message = FALSE------------------------------------
cwp <- CentWaveParam(snthresh = 3, noise = 10, ppm = 10,
                     peakwidth = c(3, 30))
swath_data <- findChromPeaksIsolationWindow(swath_data, param = cwp)
swath_data

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
mzr <- fenamiphos_ms1_peak[, c("mzmin", "mzmax")]
fenamiphos_ms1_chr <- chromatogram(swath_data, rt = rtr, mz = mzr)

rtr <- fenamiphos_ms2_peak[, c("rtmin", "rtmax")]
mzr <- fenamiphos_ms2_peak[, c("mzmin", "mzmax")]
## Get the isolationWindowTargetMz for spectra containing the m/z of the
## compound of interest
swath_data |>
filterIsolationWindow(mz = fenamiphos_mz) |>
spectra() |>
isolationWindowTargetMz() |>
table()

## -----------------------------------------------------------------------------
fenamiphos_ms2_chr <- chromatogram(
    swath_data, rt = rtr, mz = mzr, msLevel = 2L,
    isolationWindowTargetMz = rep(299.1, nrow(rtr)))

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
compareChromatograms(fenamiphos_ms2_chr[1, 1],
               fenamiphos_ms1_chr[1, 1],
               ALIGNFUNARGS = list(method = "approx"))


## ----reconstruct-ms2, message = FALSE-----------------------------------------
swath_spectra <- reconstructChromPeakSpectra(swath_data, minCor = 0.9)
swath_spectra

## -----------------------------------------------------------------------------
lengths(swath_spectra)

## -----------------------------------------------------------------------------
spectraData(swath_spectra, c("peak_id", "ms2_peak_id", "ms2_peak_cor"))

## ----fena-swath-peak----------------------------------------------------------
fenamiphos_swath_spectrum <- swath_spectra[
    swath_spectra$peak_id == rownames(fenamiphos_ms1_peak)]

## -----------------------------------------------------------------------------
fenamiphos_swath_spectrum <- addProcessing(fenamiphos_swath_spectrum,
                                           scale_fun)

## ----fena-swath-plot, fig.cap = "Mirror plot comparing the reconstructed MS2 spectrum for Fenamiphos (upper panel) against the measured spectrum from the DDA data and the Fenamiphhos spectrum from Metlin.", fig.width = 12, fig.height = 6----
par(mfrow = c(1, 2))
plotSpectraMirror(fenamiphos_swath_spectrum, ex_spectrum,
     ppm = 50, main = "against DDA")
plotSpectraMirror(fenamiphos_swath_spectrum, fenamiphos[2],
     ppm = 50, main = "against Metlin")

## -----------------------------------------------------------------------------
pk_ids <- fenamiphos_swath_spectrum$ms2_peak_id[[1]]
pk_ids

## -----------------------------------------------------------------------------
rt_range <- chromPeaks(swath_data)[pk_ids, c("rtmin", "rtmax")]
mz_range <- chromPeaks(swath_data)[pk_ids, c("mzmin", "mzmax")]

pmz <- precursorMz(fenamiphos_swath_spectrum)[1]
## Determine the isolation window target m/z
tmz <- swath_data |>
filterIsolationWindow(mz = pmz) |>
spectra() |>
isolationWindowTargetMz() |>
unique()

ms2_eics <- chromatogram(
    swath_data, rt = rt_range, mz = mz_range, msLevel = 2L,
    isolationWindowTargetMz = rep(tmz, nrow(rt_range)))

## ----fig.cap = "Overlay of EICs of chromatographic peaks used to reconstruct the MS2 spectrum for fenamiphos."----
plotChromatogramsOverlay(ms2_eics)

## ----pro-swath----------------------------------------------------------------
prochloraz_mz <- 376.0381

prochloraz_ms1_peak <- chromPeaks(swath_data, msLevel = 1L,
                                  mz = prochloraz_mz, ppm = 5)
prochloraz_ms1_peak

prochloraz_swath_spectrum <- swath_spectra[
    swath_spectra$peak_id == rownames(prochloraz_ms1_peak)]
lengths(prochloraz_swath_spectrum)

## ----pro-dda------------------------------------------------------------------
prochloraz_dda_peak <- chromPeaks(dda_data, msLevel = 1L,
                                  mz = prochloraz_mz, ppm = 5)
prochloraz_dda_peak

## ----pro-dda-ms2--------------------------------------------------------------
prochloraz_dda_spectra <- dda_spectra[
    dda_spectra$peak_id == rownames(prochloraz_dda_peak)]
prochloraz_dda_spectra

## ----pro-dda-consensus--------------------------------------------------------
prochloraz_dda_spectrum <- combineSpectra(
    prochloraz_dda_spectra, FUN = combinePeaks, ppm = 20,
    peaks = "intersect", minProp = 0.8, intensityFun = median, mzFun = median,
    f = prochloraz_dda_spectra$peak_id)

## ----prochloraz-metlin--------------------------------------------------------
prochloraz <- Spectra(
    system.file("mgf", "metlin-68898.mgf", package = "xcms"),
    source = MsBackendMgf())

## ----pro-swath-plot, fig.cap = "Mirror plot comparing the reconstructed MS2 spectrum for Prochloraz (upper panel) against the measured spectrum from the DDA data and the Prochloraz spectrum from Metlin.", fig.width = 12, fig.height = 6----
prochloraz_swath_spectrum <- addProcessing(prochloraz_swath_spectrum, scale_fun)
prochloraz_dda_spectrum <- addProcessing(prochloraz_dda_spectrum, scale_fun)

par(mfrow = c(1, 2))
plotSpectraMirror(prochloraz_swath_spectrum, prochloraz_dda_spectrum,
                  ppm = 40, main = "against DDA")
plotSpectraMirror(prochloraz_swath_spectrum, prochloraz[2],
                  ppm = 40, main = "against Metlin")

## ----fig.cap = "SWATH-derived MS2 spectrum for prochloraz."-------------------
plotSpectra(prochloraz_swath_spectrum)

## -----------------------------------------------------------------------------
library(MetaboCoreUtils)
isotopologues(peaksData(prochloraz_swath_spectrum)[[1]])

## -----------------------------------------------------------------------------
## Function to keep only the first (monoisotopic) peak for potential
## isotopologue peak groups.
rem_iso <- function(x, ...) {
    idx <- isotopologues(x)
    idx <- unlist(lapply(idx, function(z) z[-1]), use.names = FALSE)
    if (length(idx))
        x[-idx, , drop = FALSE]
    else x
}
prochloraz_swath_spectrum2 <- addProcessing(prochloraz_swath_spectrum,
                                            rem_iso)

## ----fig.cap = "SWATH MS2 spectrum for prochloraz before (left) and after deisotoping (right)."----
par(mfrow = c(1, 2))
plotSpectra(prochloraz_swath_spectrum)
plotSpectra(prochloraz_swath_spectrum2)

## -----------------------------------------------------------------------------
compareSpectra(prochloraz_swath_spectrum, prochloraz_dda_spectrum)
compareSpectra(prochloraz_swath_spectrum2, prochloraz_dda_spectrum)

## ----sessionInfo--------------------------------------------------------------
sessionInfo()

