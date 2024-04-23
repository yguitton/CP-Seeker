## ----echo=FALSE, results="hide", warning=FALSE--------------------------------
suppressPackageStartupMessages({
library('MassSpecWavelet')
})

## ----echo=FALSE, results='asis'-----------------------------------------------
print(citation("MassSpecWavelet"), style = "html")

## -----------------------------------------------------------------------------
data(exampleMS)

## -----------------------------------------------------------------------------
plotRange <- c(5000, 11000)
plot(exampleMS, xlim = plotRange, type = "l")

## -----------------------------------------------------------------------------
scales <- seq(1, 64, 2)
 wCoefs <- cwt(exampleMS, scales = scales, wavelet = "mexh")

## ----cwt,fig.align='center', fig.cap="2-D CWT coefficient image", height=10, width=20----
## Plot the 2-D CWT coefficients as image (It may take a while!)
xTickInterval <- 1000
plotRange <- c(5000, 11000)
image(
  plotRange[1]:plotRange[2],
  scales,
  wCoefs[plotRange[1]:plotRange[2],],
  col=terrain.colors(256),
  axes=FALSE,
  xlab='m/z index',
  ylab='CWT coefficient scale',
  main='CWT coefficients'
)
axis(1, at=seq(plotRange[1], plotRange[2], by=xTickInterval))
axis(2, at=c(1, seq(10, 64, by=10)))
box()

## -----------------------------------------------------------------------------
plot(exampleMS, xlim = c(8000, 9000), type = "l")

## -----------------------------------------------------------------------------
matplot(
    wCoefs[,ncol(wCoefs):1], 
    type = "l",
    col = rev(rainbow(max(scales), start = 0.7, end = 0.1, alpha = 0.5)[scales]),
    lty = 1,
    xlim = c(8000, 9000),
    xlab = "m/z index",
    ylab = "CWT coefficients"
)
legend(
    x = "topright",
    legend = sprintf("scales = %d", scales[seq(1, length(scales), length.out = 4)]),
    lty = 1,
    col = rainbow(max(scales), start = 0.7, end = 0.1)[scales[seq(1, length(scales), length.out = 4)]]
)


## -----------------------------------------------------------------------------
## Attach the raw spectrum as the first column
wCoefs <- cbind(as.vector(exampleMS), wCoefs)
colnames(wCoefs) <- c(0, scales)
localMax <- getLocalMaximumCWT(wCoefs)


## ----localMax, width=10, height=5,fig.align='center',fig.cap="Identified local maxima of CWT coefficients at each scale"----
plotLocalMax(localMax, wCoefs, range=plotRange)

## -----------------------------------------------------------------------------
ridgeList <- getRidge(localMax)

## ----ridge, width=10, height=5,fig.align='center',fig.cap="Identified ridge lines based on 2-D CWT coefficients"----
plotRidgeList(ridgeList,  wCoefs, range=plotRange)

## -----------------------------------------------------------------------------
SNR.Th <- 3
nearbyPeak <- TRUE
majorPeakInfo <- identifyMajorPeaks(
  exampleMS,
  ridgeList,
  wCoefs,
  SNR.Th = SNR.Th,
  nearbyPeak=nearbyPeak
)
## Plot the identified peaks
peakIndex <- majorPeakInfo$peakIndex

## ----peak, width=10, height=5,fig.align='center',fig.cap='Identified peaks'----
plotPeak(
  exampleMS,
  peakIndex,
  range=plotRange,
  main=paste('Identified peaks with SNR >', SNR.Th)
)

## -----------------------------------------------------------------------------
data(exampleMS)
SNR.Th <- 3
nearbyPeak <- TRUE
peakInfo <- peakDetectionCWT(exampleMS, SNR.Th=SNR.Th, nearbyPeak=nearbyPeak)
majorPeakInfo = peakInfo$majorPeakInfo
peakIndex <- majorPeakInfo$peakIndex
plotRange <- c(5000, length(exampleMS))

## ----peak1, width=10, height=5, fig.cap='Identified peaks', fig.align='center'----
plotPeak(
  exampleMS,
  peakIndex,
  range=plotRange,
  log='x',
  main=paste('Identified peaks with SNR >', SNR.Th)
)

## -----------------------------------------------------------------------------
peakSNR <- majorPeakInfo$peakSNR
allPeakIndex <- majorPeakInfo$allPeakIndex

## ----SNR, width=10, height=5,fig.align='center',fig.cap='Estimated Signal to Noise Ration (SNR) of the peaks'----
plotRange <- c(5000, 36000)
selInd <- which(allPeakIndex >= plotRange[1] & allPeakIndex < plotRange[2])
plot(
  allPeakIndex[selInd],
  peakSNR[selInd],
  type='h',
  xlab='m/z Index',
  ylab='Signal to Noise Ratio (SNR)',
  log='x'
)
points(peakIndex, peakSNR[names(peakIndex)], type='h', col='red')
title('Signal to Noise Ratio (SNR) of the peaks (CWT method)')

## -----------------------------------------------------------------------------
betterPeakInfo <- tuneInPeakInfo(exampleMS, majorPeakInfo)

## ----peak2, width=10, height=5,fig.align='center',fig.cap='Identified peaks with refined peak center position'----
plotRange <- c(5000, 11000)
plot(
  plotRange[1]:plotRange[2],
  exampleMS[plotRange[1]:plotRange[2]],
  type='l',
  log='x',
  xlab='m/z Index',
  ylab='Intensity'
)
abline(v=betterPeakInfo$peakCenterIndex, col='red')

## -----------------------------------------------------------------------------
sessionInfo()

