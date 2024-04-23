## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  if (!requireNamespace("BiocManager", quietly = TRUE))
#      install.packages("BiocManager")
#  BiocManager::install("BiocBaseUtils")

## ----include=TRUE,results="hide",message=FALSE,warning=FALSE------------------
library(BiocBaseUtils)

## -----------------------------------------------------------------------------
isTRUEorFALSE(TRUE)
isTRUEorFALSE(FALSE)
isTRUEorFALSE(NA, na.ok = TRUE)

## -----------------------------------------------------------------------------
isScalarCharacter(LETTERS)
isScalarCharacter("L")
isCharacter(LETTERS)
isCharacter(NA_character_, na.ok = TRUE)
isZeroOneCharacter("")
isZeroOneCharacter("", zchar = TRUE)

## -----------------------------------------------------------------------------
isScalarInteger(1L)
isScalarInteger(1)

isScalarNumber(1)
isScalarNumber(1:2)

## -----------------------------------------------------------------------------
setClass("A", representation = representation(slot1 = "numeric"))
aclass <- new("A", slot1 = 1:10)
aclass

## -----------------------------------------------------------------------------
aclass <- setSlots(aclass, slot1 = 11:20)
aclass

## -----------------------------------------------------------------------------
setMethod("show", signature = "A", function(object) {
    s1info <- getElement(object, "slot1")
    cat("A sequence:", selectSome(s1info))
})
aclass

## -----------------------------------------------------------------------------
sessionInfo()

