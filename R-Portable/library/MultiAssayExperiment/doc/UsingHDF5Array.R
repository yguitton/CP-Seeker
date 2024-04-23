## ----include=TRUE,results="hide",message=FALSE,warning=FALSE------------------
library(MultiAssayExperiment)
library(HDF5Array)
library(SummarizedExperiment)

## -----------------------------------------------------------------------------
smallMatrix <- matrix(rnorm(10e5), ncol = 20)

## -----------------------------------------------------------------------------
rownames(smallMatrix) <- paste0("GENE", seq_len(nrow(smallMatrix)))
colnames(smallMatrix) <- paste0("SampleID", seq_len(ncol(smallMatrix)))

## -----------------------------------------------------------------------------
smallMatrix <- DelayedArray(smallMatrix)
class(smallMatrix)
# show method
smallMatrix

dim(smallMatrix)

## -----------------------------------------------------------------------------
testh5 <- tempfile(fileext = ".h5")
writeHDF5Array(smallMatrix, filepath = testh5, name = "smallMatrix",
    with.dimnames = TRUE)

## -----------------------------------------------------------------------------
h5ls(testh5)

## -----------------------------------------------------------------------------
hdf5Data <- HDF5ArraySeed(file = testh5, name = "smallMatrix")
newDelayedMatrix <- DelayedArray(hdf5Data)
class(newDelayedMatrix)
newDelayedMatrix

## -----------------------------------------------------------------------------
HDF5MAE <- MultiAssayExperiment(experiments = list(smallMatrix = smallMatrix))
sampleMap(HDF5MAE)
colData(HDF5MAE)

## -----------------------------------------------------------------------------
HDF5SE <- SummarizedExperiment(assays = smallMatrix)
assay(HDF5SE)
MultiAssayExperiment(list(HDF5SE = HDF5SE))

## -----------------------------------------------------------------------------
sessionInfo()

