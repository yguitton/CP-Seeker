## ----style, echo = FALSE, results = 'asis'------------------------------------
BiocStyle::markdown()

## ----message = FALSE----------------------------------------------------------
library("MsCoreUtils")
ls(pos = "package:MsCoreUtils")

## -----------------------------------------------------------------------------
x <- matrix(rnorm(30), nrow = 3)
colnames(x) <- letters[1:10]
rownames(x) <- LETTERS[1:3]
x
robustSummary(x)

## ----sessioninfo, echo=FALSE--------------------------------------------------
sessionInfo()

