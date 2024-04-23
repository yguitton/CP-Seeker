## -----------------------------------------------------------------------------
library(MassSpecWavelet)

## -----------------------------------------------------------------------------
onetwothree <- c(1,2,3)
x <- c(0, onetwothree, onetwothree, 0, 0, 0, onetwothree, 1, onetwothree, onetwothree, 0)
plot(x, type = "o", main = "Five peaks are expected")

## -----------------------------------------------------------------------------
options("MassSpecWavelet.localMaximum.algorithm" = "new")
local_max <- which(localMaximum(x, winSize = 4) > 0)
plot(x, type = "o", main = "With the new algorithm, 5/5 peaks are found")
points(local_max, x[local_max], col = "red", pch = 20)


## -----------------------------------------------------------------------------
options("MassSpecWavelet.localMaximum.algorithm" = "classic")
local_max <- which(localMaximum(x, winSize = 4) > 0)
plot(x, type = "o", main = "With the classic algorithm, 3/5 peaks are found")
points(local_max, x[local_max], col = "red", pch = 20)

## -----------------------------------------------------------------------------
x <- c(0, 1, 2, 3, 3, 3, 3, 2, 1, 0, 3, 0, 3, 3, 3, 3, 3, 0)
x <- c(x, 0, 1, 2, 3, 3, 3, 2, 1, 0, 3, 0, 0, 0, 3, 3, 3, 0, 0)
options("MassSpecWavelet.localMaximum.algorithm" = "classic")
local_max_classic <- which(localMaximum(x, winSize = 5) > 0)
options("MassSpecWavelet.localMaximum.algorithm" = "new")
local_max_new <- which(localMaximum(x, winSize = 5) > 0)
par(mfrow = c(2, 1))
plot(x, type = "o", main = "With the classic algorithm, 2/6 peaks are found")
points(local_max_classic, x[local_max_classic], col = "red", pch = 20)
plot(x, type = "o", main = "With the new algorithm, 6/6 peaks are found")
points(local_max_new, x[local_max_new], col = "blue", pch = 20)


## ----benchmark, fig.cap="CPU time vs length, for several algorithms and windows"----
# Run this interactively
set.seed(5413L)
winSizes <- c(5, 31, 301)
xlengths <- c(20, 200, 2000, 20000, 200000)
out <- vector("list", length(winSizes) * length(xlengths))
i <- 0L
for (winSize in winSizes) {
    for (xlength in xlengths) {
        i <- i + 1L
        x <- round(10*runif(xlength), 1)*10
        bm1 <- as.data.frame(
                bench::mark(
                classic = {
                    options(MassSpecWavelet.localMaximum.algorithm = "classic")
                    localMaximum(x, winSize = winSize)
                    
                },
                faster = {
                    options(MassSpecWavelet.localMaximum.algorithm = "faster")
                    localMaximum(x, winSize = winSize)
                    
                },
                check = TRUE,
                filter_gc = FALSE,
                time_unit = "ms"
            )
        )
        bm2 <- as.data.frame(
                bench::mark(
                new = {
                    options(MassSpecWavelet.localMaximum.algorithm = "new")
                    localMaximum(x, winSize = winSize)
                    
                },
                check = FALSE,
                filter_gc = FALSE,
                time_unit = "ms"
            )
        )
        out[[i]] <- data.frame(
            algorithm = c(as.character(bm1$expression), as.character(bm2$expression)),
            min_cpu_time_ms = c(as.numeric(bm1$min), as.numeric(bm2$min)),
            xlength = xlength,
            winSize = winSize
        )
    }
}
out2 <- do.call(rbind, out)
plot(
    x = out2$xlength,
    y = out2$min_cpu_time_ms,
    col = ifelse(out2$algorithm == "new", "red", ifelse(out2$algorithm == "faster", "darkgreen", "blue")),
    pch = ifelse(out2$winSize == 5, 1, ifelse(out2$winSize == 31, 2, 3)),
    log = "xy",
    xlab = "Signal length",
    ylab = "Min CPU time (ms)"
)
legend(
    "topleft",
    legend = c("New algorithm", "Classic algorithm", "Faster algorithm",
               "winSize = 5", "winSize = 31", "winSize = 301"),
    lty = c(1,1,1, 0, 0, 0),
    pch = c(NA, NA, NA,1, 2, 3),
    col = c("red", "blue", "darkgreen", "black", "black", "black")
)

## -----------------------------------------------------------------------------
data(exampleMS)
scales <- seq(1, 64, 1)
wCoefs <- cwt(exampleMS, scales = scales, wavelet = "mexh")

## -----------------------------------------------------------------------------
plotRange <- c(8000, 9000)

## -----------------------------------------------------------------------------
plot(exampleMS, xlim = plotRange, type = "l")

## -----------------------------------------------------------------------------
matplot(
    wCoefs[,ncol(wCoefs):1], 
    type = "l",
    col = rev(rainbow(64, start = 0.7, end = 0.1, alpha = 0.5)[scales]),
    lty = 1,
    xlim = c(8000, 9000),
    xlab = "m/z index",
    ylab = "CWT coefficients"
)
legend(
    x = "topright",
    legend = sprintf("scales = %d", scales[seq(1, length(scales), length.out = 4)]),
    lty = 1,
    col = rainbow(64, start = 0.7, end = 0.1)[scales[seq(1, length(scales), length.out = 4)]]
)


## -----------------------------------------------------------------------------
options(MassSpecWavelet.localMaximum.algorithm = "new")
localMax_new <- getLocalMaximumCWT(wCoefs)
options(MassSpecWavelet.localMaximum.algorithm = "classic")
localMax_classic <- getLocalMaximumCWT(wCoefs)

## ----localMax, width=10, height=8,fig.align='center',fig.cap="Identified local maxima of CWT coefficients at each scale"----
par(mfrow = c(2,1))
plotLocalMax(localMax_classic, NULL, range=c(plotRange[1], plotRange[2]), main = "classic")
plotLocalMax(localMax_new, NULL, range=c(plotRange[1], plotRange[2]), main = "new")

## -----------------------------------------------------------------------------
sessionInfo()

