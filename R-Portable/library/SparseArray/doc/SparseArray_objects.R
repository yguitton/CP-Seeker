## ----setup, include=FALSE-----------------------------------------------------
library(BiocStyle)

## ----eval=FALSE---------------------------------------------------------------
#  if (!requireNamespace("BiocManager", quietly=TRUE))
#      install.packages("BiocManager")
#  BiocManager::install("SparseArray")

## ----message=FALSE------------------------------------------------------------
library(SparseArray)

## -----------------------------------------------------------------------------
m <- matrix(0L, nrow=6, ncol=4)
m[c(1:2, 8, 10, 15:17, 24)] <- (1:8)*10L
svt1 <- as(m, "SVT_SparseArray")
svt1

a <- array(0L, 5:3)
a[c(1:2, 8, 10, 15:17, 20, 24, 40, 56:60)] <- (1:15)*10L
svt2 <- as(a, "SVT_SparseArray")
svt2

## -----------------------------------------------------------------------------
svt1 <- SVT_SparseArray(m)
svt2 <- SVT_SparseArray(a)

## -----------------------------------------------------------------------------
svt1 <- as(m, "SparseArray")
svt2 <- as(a, "SparseArray")

## -----------------------------------------------------------------------------
svt1 <- SparseArray(m)
svt2 <- SparseArray(a)

## -----------------------------------------------------------------------------
as.array(svt1)  # same as as.matrix(svt1)

as.array(svt2)

## -----------------------------------------------------------------------------
dim(svt2)

length(svt2)

dimnames(svt2) <- list(NULL, letters[1:4], LETTERS[1:3])
svt2

## -----------------------------------------------------------------------------
type(svt1)

type(svt1) <- "double"
svt1

is_sparse(svt1)

## -----------------------------------------------------------------------------
## Get the number of nonzero array elements in 'svt1':
nzcount(svt1)

## Extract the "linear indices" of the nonzero array elements in 'svt1':
nzwhich(svt1)

## Extract the "array indices" (a.k.a. "array coordinates") of the
## nonzero array elements in 'svt1':
nzwhich(svt1, arr.ind=TRUE)

## Extract the values of the nonzero array elements in 'svt1' and return
## them in a vector "parallel" to 'nzwhich(svt1)':
#nzvals(svt1)  # NOT READY YET!

sparsity(svt1)

## -----------------------------------------------------------------------------
svt2[5:3, , "C"]

## -----------------------------------------------------------------------------
type(svt2)
svt2[5, 1, 3] <- NaN
type(svt2)

## -----------------------------------------------------------------------------
anyNA(svt2)

range(svt2, na.rm=TRUE)

mean(svt2, na.rm=TRUE)

var(svt2, na.rm=TRUE)

## -----------------------------------------------------------------------------
signif((svt1^1.5 + svt1) %% 100 - 0.6 * svt1, digits=2)

## -----------------------------------------------------------------------------
randomSparseArray(c(5, 6, 2), density=0.5)

poissonSparseArray(c(5, 6, 2), density=0.5)

## -----------------------------------------------------------------------------
t(svt1)

## -----------------------------------------------------------------------------
aperm(svt2)

## -----------------------------------------------------------------------------
svt3 <- poissonSparseMatrix(6, 2, density=0.5)

cbind(svt1, svt3)

## -----------------------------------------------------------------------------
svt4a <- poissonSparseArray(c(5, 6, 2), density=0.4)
svt4b <- poissonSparseArray(c(5, 6, 5), density=0.2)
svt4c <- poissonSparseArray(c(5, 6, 4), density=0.2)
abind(svt4a, svt4b, svt4c)

svt5a <- aperm(svt4a, c(1, 3:2))
svt5b <- aperm(svt4b, c(1, 3:2))
svt5c <- aperm(svt4c, c(1, 3:2))
abind(svt5a, svt5b, svt5c, along=2)

## -----------------------------------------------------------------------------
m6 <- matrix(0L, nrow=5, ncol=6, dimnames=list(letters[1:5], LETTERS[1:6]))
m6[c(2, 6, 12:17, 22:30)] <- 101:117
svt6 <- SparseArray(m6)

svt6 %*% svt3

## -----------------------------------------------------------------------------
crossprod(svt3)

## -----------------------------------------------------------------------------
colVars(svt6)

## -----------------------------------------------------------------------------
colVars(svt2)
colVars(svt2, dims=2)
colAnyNAs(svt2)
colAnyNAs(svt2, dims=2)

## -----------------------------------------------------------------------------
rowsum(svt6, group=c(1:3, 2:1))

## -----------------------------------------------------------------------------
csv_file <- tempfile()
writeSparseCSV(m6, csv_file)

## -----------------------------------------------------------------------------
readSparseCSV(csv_file)

## -----------------------------------------------------------------------------
sessionInfo()

