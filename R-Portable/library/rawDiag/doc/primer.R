### R code from vignette source 'primer.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: primer.Rnw:55-57
###################################################
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
library(tidyverse)


###################################################
### code chunk number 2: primer.Rnw:115-116 (eval = FALSE)
###################################################
## help(package="rawDiag")


###################################################
### code chunk number 3: primer.Rnw:120-121
###################################################
library(rawDiag)


###################################################
### code chunk number 4: primer.Rnw:135-137
###################################################
data(WU163763)
stopifnot(is.rawDiag(WU163763))


###################################################
### code chunk number 5: WU163763
###################################################
data("WU163763")
is.rawDiag(WU163763)
names(WU163763)


###################################################
### code chunk number 6: RawFileReader
###################################################
rawfile <- file.path(path.package(package = "rawDiag"),
                     "extdata", 'sample.raw')
system.time(RAW <- read.raw(file = rawfile))
summary.rawDiag(RAW)


###################################################
### code chunk number 7: RawFileReader2
###################################################
dim(RAW)
RAW <- read.raw(file = rawfile, rawDiag = FALSE)
dim(RAW)


###################################################
### code chunk number 8: primer.Rnw:170-171 (eval = FALSE)
###################################################
## ?read.raw


###################################################
### code chunk number 9: mzR
###################################################
library(mzR); 

mzML <- "04_S174020_5000_5010.mzML"
mzML <- file.path(path.package(package = "rawDiag"), "extdata", mzML)
system.time(RAW <- rawDiag:::as.rawDiag.mzR(openMSfile(mzML)))
summary.rawDiag(RAW)
RAW$scanNumber


###################################################
### code chunk number 10: usage
###################################################
data(WU163763)
stopifnot(is.rawDiag(WU163763))


###################################################
### code chunk number 11: filter
###################################################
  library(tidyverse)
df <- dplyr::filter(WU163763, 
  filename == "04_S174020" |
  filename == "05_S174020" |
  filename == "09_S174020")


###################################################
### code chunk number 12: TIC
###################################################
print(gp <- PlotTicBasepeak(df, method = "overlay"))


###################################################
### code chunk number 13: PlotCycleTime
###################################################
print(gp <- PlotCycleTime(df, method = "trellis") +
  facet_wrap(~ filename, ncol =1))


###################################################
### code chunk number 14: PlotCycleLoad
###################################################
print(gp <- PlotCycleLoad(df, method = "trellis"))


###################################################
### code chunk number 15: PlotInjectionTime
###################################################
print(gp <- PlotInjectionTime(subset(df, MSOrder %in% "Ms2"), 
    method = "violin") 
  )


###################################################
### code chunk number 16: shiny (eval = FALSE)
###################################################
## # install.packages("shiny"); install.packages("DT")
## library(shiny)
## rawDiag_shiny <- system.file('shiny', 'demo', package = 'rawDiag')
## shiny::runApp(rawDiag_shiny, display.mode = 'normal')


###################################################
### code chunk number 17: primer.Rnw:313-329 (eval = FALSE)
###################################################
## library(parallel)
## 
## f <- list.files()
## f <- f[grep("raw$", f)]
## 
## b <- lapply(1, function(x){rawDiag:::benchmark_raw(f,
##   exe="~/RiderProjects/fgcz-raw/bin/Debug/fgcz_raw.exe")})
## 
## b <- plyr::rbind.fill(lapply(b, plyr::rbind.fill))
## 
## b$overall.runtime <- as.integer(format(b$end.time, "%s")) - 
##   as.integer(format(b$start.time, "%s"))
## 
## b$system <- "Linux"
## b.Linux <- b
## save(b.Linux, file='benchmark.RData')


###################################################
### code chunk number 18: benchmark-time
###################################################
rawDiag:::.technote_benchmark_figure_1() 


###################################################
### code chunk number 19: benchmark-throuput
###################################################
rawDiag:::.technote_benchmark_figure_2()


###################################################
### code chunk number 20: sessioninfo
###################################################
toLatex(sessionInfo())


