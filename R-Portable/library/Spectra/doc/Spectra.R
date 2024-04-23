## ----style, echo = FALSE, results = 'asis', message=FALSE---------------------
BiocStyle::markdown()

## ----echo = FALSE, message = FALSE--------------------------------------------
library(Spectra)
library(BiocStyle)

## ----spectra-dataframe, message = FALSE---------------------------------------
library(Spectra)

spd <- DataFrame(
    msLevel = c(2L, 2L, 2L),
    polarity = c(1L, 1L, 1L),
    id = c("HMDB0000001", "HMDB0000001", "HMDB0001847"),
    name = c("1-Methylhistidine", "1-Methylhistidine", "Caffeine"))

## Assign m/z and intensity values.
spd$mz <- list(
    c(109.2, 124.2, 124.5, 170.16, 170.52),
    c(83.1, 96.12, 97.14, 109.14, 124.08, 125.1, 170.16),
    c(56.0494, 69.0447, 83.0603, 109.0395, 110.0712,
      111.0551, 123.0429, 138.0662, 195.0876))
spd$intensity <- list(
    c(3.407, 47.494, 3.094, 100.0, 13.240),
    c(6.685, 4.381, 3.022, 16.708, 100.0, 4.565, 40.643),
    c(0.459, 2.585, 2.446, 0.508, 8.968, 0.524, 0.974, 100.0, 40.994))

sps <- Spectra(spd)
sps

## ----spectra-msbackendmzr, message = FALSE------------------------------------
fls <- dir(system.file("sciex", package = "msdata"), full.names = TRUE)
sps_sciex <- Spectra(fls, source = MsBackendMzR())
sps_sciex

## ----spectravariables---------------------------------------------------------
spectraVariables(sps)
spectraVariables(sps_sciex)

## ----mslevel-sps--------------------------------------------------------------
msLevel(sps)
rtime(sps)

## ----rtime-spssciex-----------------------------------------------------------
head(rtime(sps_sciex))

## ----dollar-extract-----------------------------------------------------------
sps$name
sps$msLevel

## ----dollar-set---------------------------------------------------------------
sps$centroided <- TRUE

centroided(sps)

## ----new-spectra-variable-----------------------------------------------------
sps$splash <- c(
    "splash10-00di-0900000000-037d24a7d65676b7e356",
    "splash10-00di-0900000000-03e99316bd6c098f5d11",
    "splash10-000i-0900000000-9af60e39c843cb715435")

## ----mz-intensity-------------------------------------------------------------
mz(sps)
intensity(sps)

## -----------------------------------------------------------------------------
peaksVariables(sps)

## ----peaks--------------------------------------------------------------------
pks <- peaksData(sps)
pks[[1]]

## ----as-----------------------------------------------------------------------
as(sps, "SimpleList")

## ----spectradata--------------------------------------------------------------
spectraData(sps, columns = c("msLevel", "id", "name"))

## ----dataOrigin-sps-----------------------------------------------------------
dataOrigin(sps)

## ----dataOrigin-sciex---------------------------------------------------------
head(basename(dataOrigin(sps_sciex)))

## ----dataStorage--------------------------------------------------------------
dataStorage(sps)
head(basename(dataStorage(sps_sciex)))

## -----------------------------------------------------------------------------
## Extract the full data from a spectrum
spd <- spectraData(sps, columns = union(spectraVariables(sps),
                                        peaksVariables(sps)))
## Add a new column with a *annotation* for each peak
spd$peak_anno <- list(c("a", NA_character_, "b", "c", "d"),
                      c("a", "b", "c", "d", "e", "f", "g"),
                      c("a", "b", "c", "d", "e", "f", "g", "h", "i"))
## lengths have to match:
lengths(spd$peak_anno)
lengths(spd$mz)

## -----------------------------------------------------------------------------
sps2 <- Spectra(spd, backend = MsBackendMemory(),
                peaksVariables = c("mz", "intensity", "peak_anno"))
peaksVariables(sps2)

## -----------------------------------------------------------------------------
peaksData(sps2, columns = peaksVariables(sps2))[[2L]]

## -----------------------------------------------------------------------------
## Peak annotations for the first spectrum
peaksData(sps2, "peak_anno")[[1L]]
## Peak annotations for the second spectrum
peaksData(sps2, "peak_anno")[[2L]]

## -----------------------------------------------------------------------------
sps2$peak_anno

## ----filterfile-filterrt------------------------------------------------------
fls <- unique(dataOrigin(sps_sciex))
file_2 <- filterDataOrigin(sps_sciex, dataOrigin = fls[2])
length(file_2)

sps_sub <- filterRt(file_2, rt = c(175, 189))
length(sps_sub)

## ----subset-square-bracket----------------------------------------------------
sps_sciex[sps_sciex$dataOrigin == fls[2] &
          sps_sciex$rtime >= 175 &
          sps_sciex$rtime <= 189]

## ----subset-filter-pipes------------------------------------------------------
sps_sciex |>
    filterDataOrigin(fls[2]) |>
    filterRt(c(175, 189))

## ----caf----------------------------------------------------------------------
caf_df <- DataFrame(msLevel = 2L, name = "Caffeine",
                    id = "HMDB0001847",
                    instrument = "Agilent 1200 RRLC; Agilent 6520 QTOF",
                    splash = "splash10-0002-0900000000-413259091ba7edc46b87",
                    centroided = TRUE)
caf_df$mz <- list(c(110.0710, 138.0655, 138.1057, 138.1742, 195.9864))
caf_df$intensity <- list(c(3.837, 32.341, 0.84, 0.534, 100))

caf <- Spectra(caf_df)

## ----combine------------------------------------------------------------------
sps <- concatenateSpectra(sps, caf)
sps

## ----merge-spectravariables---------------------------------------------------
spectraVariables(sps)

## ----merge-add-column---------------------------------------------------------
sps$instrument

## ----replaceintensities-------------------------------------------------------
sps_rep <- replaceIntensitiesBelow(sps, threshold = 10, value = 0)

## ----replaceintensities-intensity---------------------------------------------
intensity(sps_rep)

## ----clean--------------------------------------------------------------------
sps_rep <- filterIntensity(sps_rep, intensity = c(0.1, Inf))

## ----clean-intensity----------------------------------------------------------
intensity(sps_rep)

## ----processing-queue---------------------------------------------------------
sps_rep

## ----define-function----------------------------------------------------------
## Define a function that takes a matrix as input, divides the second
## column by parameter y and returns it. Note that ... is required in
## the function's definition.
divide_intensities <- function(x, y, ...) {
    x[, 2] <- x[, 2] / y
    x
}

## Add the function to the procesing queue
sps_2 <- addProcessing(sps_rep, divide_intensities, y = 2)
sps_2

## ----custom-processing--------------------------------------------------------
intensity(sps_2)
intensity(sps_rep)

## ----return-max-peak----------------------------------------------------------
max_peak <- function(x, ...) {
    unname(x[which.max(x[, 2]), , drop = FALSE])
}

sps_2 <- addProcessing(sps_rep, max_peak)
lengths(sps_2)
intensity(sps_2)

## ----set-precursor-mz---------------------------------------------------------
sps_rep$precursorMz <- c(170.5, 170.5, 195.1, 195.1)

neutral_loss <- function(x, precursorMz, ...) {
    x[, "mz"] <- precursorMz - x[, "mz"]
    x[order(x[, "mz"]), , drop = FALSE]
}

## ----neutral-loss-------------------------------------------------------------
sps_3 <- addProcessing(sps_rep, neutral_loss,
                       spectraVariables = "precursorMz")
mz(sps_rep)
mz(sps_3)

## ----neutral-loss2------------------------------------------------------------
neutral_loss <- function(x, spectrumMsLevel, precursorMz, ...) {
    if (spectrumMsLevel == 2L) {
        x[, "mz"] <- precursorMz - x[, "mz"]
        x <- x[order(x[, "mz"]), , drop = FALSE]
    }
    x
}
sps_3 <- addProcessing(sps_rep, neutral_loss,
                       spectraVariables = c("msLevel", "precursorMz"))
mz(sps_3)

## -----------------------------------------------------------------------------
library(MsCoreUtils)
remove_precursor <- function(x, precursorMz, tolerance = 0.1, ppm = 0, ...) {
    if (!is.na(precursorMz)) {
        keep <- is.na(closest(x[, "mz"], precursorMz, tolerance = tolerance,
                             ppm = ppm, .check = FALSE))
        x[keep, , drop = FALSE]
    } else x
}

## -----------------------------------------------------------------------------
sps_4 <- addProcessing(sps_rep, remove_precursor,
                       spectraVariables = "precursorMz")

peaksData(sps_4) |> as.list()

## -----------------------------------------------------------------------------
peaksData(sps_rep) |> as.list()

## -----------------------------------------------------------------------------
sps_4 <- addProcessing(sps_rep, remove_precursor, tolerance = 0.6,
                       spectraVariables = "precursorMz")
peaksData(sps_4) |> as.list()

## ----reset--------------------------------------------------------------------
sps_2_rest <- reset(sps_2)

intensity(sps_2_rest)
intensity(sps)

## ----applyProcessing----------------------------------------------------------
length(sps_rep@processingQueue)

sps_rep <- applyProcessing(sps_rep)
length(sps_rep@processingQueue)
sps_rep

## ----plotspectra, fig.width = 8, fig.height = 8-------------------------------
plotSpectra(sps, main = sps$name)

## ----plotspectra-label, fig.width = 8, fig.height = 8-------------------------
plotSpectra(sps, main = sps$name,
            labels = function(z) format(mz(z)[[1L]], digits = 4),
            labelSrt = -30, labelPos = 2, labelOffset = 0.1)

## ----plotspectra-label-int, fig.width = 8, fig.height = 8---------------------
mzLabel <- function(z) {
    z <- peaksData(z)[[1L]]
    lbls <- format(z[, "mz"], digits = 4)
    lbls[z[, "intensity"] < 30] <- ""
    lbls
}
plotSpectra(sps, main = sps$name, labels = mzLabel,
            labelSrt = -30, labelPos = 2, labelOffset = 0.1)

## ----plotspectraoverlay, fig.width = 6, fig.height = 6------------------------
cols <- c("#E41A1C80", "#377EB880", "#4DAF4A80", "#984EA380")
plotSpectraOverlay(sps, lwd = 2, col = cols)
legend("topleft", col = cols, legend = sps$name, pch = 15)

## ----plotspectramirror, fig.width = 6, fig.height = 6-------------------------
plotSpectraMirror(sps[1], sps[3])

## ----plotspectramirror-ppm, fig.width = 12, fig.height = 6--------------------
par(mfrow = c(1, 2))
plotSpectraMirror(sps[1], sps[2], main = "1-Methylhistidine", ppm = 50)
plotSpectraMirror(sps[3], sps[4], main = "Caffeine", ppm = 50)

## -----------------------------------------------------------------------------
sps_agg <- combineSpectra(sps, f = sps$name)

## ----fig.width = 4, fig.height = 8--------------------------------------------
plotSpectra(sps_agg, main = sps_agg$name)

## ----fig.width = 4, fig.height = 8--------------------------------------------
sps_agg <- combineSpectra(sps, f = sps$name, peaks = "intersect", minProp = 1)
plotSpectra(sps_agg, main = sps_agg$name)

## ----fig.width = 4, fig.height = 8--------------------------------------------
sps_agg <- combineSpectra(sps, f = sps$name, peaks = "intersect",
                          minProp = 1, tolerance = 0.2)
plotSpectra(sps_agg, main = sps_agg$name)

## -----------------------------------------------------------------------------
#' function to select and return the peak matrix with the largest tic from
#' the provided list of peak matrices.
maxTic <- function(x, ...) {
    tic <- vapply(x, function(z) sum(z[, "intensity"], na.rm = TRUE),
                  numeric(1))
    x[[which.max(tic)]]
}

## ----fig.width = 4, fig.height = 8--------------------------------------------
sps_agg <- combineSpectra(sps, f = sps$name, FUN = maxTic)
plotSpectra(sps_agg, main = sps_agg$name)

## ----comparespectra-----------------------------------------------------------
compareSpectra(sps, ppm = 50)

## -----------------------------------------------------------------------------
library(msentropy)
compareSpectra(sps, MAPFUN = joinPeaksNone, FUN = msentropy_similarity,
               ms2_tolerance_in_ppm = 50, ms2_tolerance_in_da = -1)

## -----------------------------------------------------------------------------
sps_bin <- Spectra::bin(sps, binSize = 0.1)

## -----------------------------------------------------------------------------
lengths(sps_bin)

## -----------------------------------------------------------------------------
intensity(sps_bin)

## -----------------------------------------------------------------------------
intmat <- do.call(rbind, intensity(sps_bin))

## -----------------------------------------------------------------------------
zeros <- colSums(intmat) == 0
intmat <- intmat[, !zeros]
intmat

## -----------------------------------------------------------------------------
colnames(intmat) <- mz(sps_bin)[[1L]][!zeros]

## -----------------------------------------------------------------------------
heatmap(intmat)

## ----export-------------------------------------------------------------------
fl <- tempfile()
export(sps, MsBackendMzR(), file = fl)

## ----export-import------------------------------------------------------------
sps_im <- Spectra(backendInitialize(MsBackendMzR(), fl))
spectraVariables(sps)[!spectraVariables(sps) %in% spectraVariables(sps_im)]

## ----export-twofiles----------------------------------------------------------
fls <- c(tempfile(), tempfile())
export(sps, MsBackendMzR(), file = fls[c(1, 2, 1, 2)])

## ----setbackend---------------------------------------------------------------
print(object.size(sps_sciex), units = "Mb")
sps_sciex <- setBackend(sps_sciex, MsBackendMemory())
sps_sciex

## ----memory-after-import------------------------------------------------------
print(object.size(sps_sciex), units = "Mb")

## ----new-datastorage----------------------------------------------------------
head(dataStorage(sps_sciex))
head(basename(dataOrigin(sps_sciex)))

## -----------------------------------------------------------------------------
tmp <- Spectra()
backendBpparam(tmp)

## -----------------------------------------------------------------------------
backendBpparam(MsBackendDataFrame(), SnowParam(2))

## ----hdf5---------------------------------------------------------------------
library(msdata)
fl <- proteomics(full.names = TRUE)[5]

sps_tmt <- Spectra(fl, backend = MsBackendHdf5Peaks(), hdf5path = tempdir())
head(basename(dataStorage(sps_tmt)))

## ----si-----------------------------------------------------------------------
sessionInfo()

