## ----include=TRUE,results="hide",message=FALSE,warning=FALSE------------------
library(MultiAssayExperiment)
library(S4Vectors)

## -----------------------------------------------------------------------------
data(miniACC)
miniACC

## -----------------------------------------------------------------------------
colData(miniACC)[1:4, 1:4]
table(miniACC$race)

## -----------------------------------------------------------------------------
experiments(miniACC)

## -----------------------------------------------------------------------------
sampleMap(miniACC)

## -----------------------------------------------------------------------------
metadata(miniACC)

## ----results='hide'-----------------------------------------------------------
miniACC[c("MAPK14", "IGFBP2"), , ]

## ----results='hide'-----------------------------------------------------------
stg4 <- miniACC$pathologic_stage == "stage iv"
# remove NA values from vector
miniACC[, stg4 & !is.na(stg4), ]

## ----results='hide'-----------------------------------------------------------
miniACC[, , "RNASeq2GeneNorm"]

## -----------------------------------------------------------------------------
miniACC[[1L]]  #or equivalently, miniACC[["RNASeq2GeneNorm"]]

## -----------------------------------------------------------------------------
summary(complete.cases(miniACC))

## -----------------------------------------------------------------------------
accmatched = intersectColumns(miniACC)

## -----------------------------------------------------------------------------
colnames(accmatched)

## -----------------------------------------------------------------------------
accmatched2 <- intersectRows(miniACC[, , c("RNASeq2GeneNorm",
                                           "gistict",
                                           "Mutations")])
rownames(accmatched2)

## -----------------------------------------------------------------------------
class(assay(miniACC))

## -----------------------------------------------------------------------------
assays(miniACC)

## -----------------------------------------------------------------------------
longFormat(miniACC[c("TP53", "CTNNB1"), , ],
           colDataCols = c("vital_status", "days_to_death"))

## -----------------------------------------------------------------------------
wideFormat(miniACC[c("TP53", "CTNNB1"), , ],
           colDataCols = c("vital_status", "days_to_death"))

## -----------------------------------------------------------------------------
MultiAssayExperiment(experiments=experiments(miniACC),
    colData=colData(miniACC),
    sampleMap=sampleMap(miniACC),
    metadata=metadata(miniACC))

## -----------------------------------------------------------------------------
miniACC2 <- c(miniACC,
    log2rnaseq = log2(assays(miniACC)$RNASeq2GeneNorm), mapFrom=1L)
assays(miniACC2)

## -----------------------------------------------------------------------------
library(UpSetR)
upsetSamples(miniACC)

## ----message=FALSE------------------------------------------------------------
library(survival)
library(survminer)

coldat <- as.data.frame(colData(miniACC))
coldat$y <- Surv(miniACC$days_to_death, miniACC$vital_status)
colData(miniACC) <- DataFrame(coldat)

## -----------------------------------------------------------------------------
miniACC <- miniACC[, complete.cases(coldat$y), ]
coldat <- as(colData(miniACC), "data.frame")

## -----------------------------------------------------------------------------
fit <- survfit(y ~ pathology_N_stage, data = coldat)
ggsurvplot(fit, data = coldat, risk.table = TRUE)

## -----------------------------------------------------------------------------
wideacc <- wideFormat(miniACC["EZH2", , ],
    colDataCols = c("vital_status", "days_to_death", "pathology_N_stage"))
wideacc$y <- Surv(wideacc$days_to_death, wideacc$vital_status)
head(wideacc)

## -----------------------------------------------------------------------------
coxph(Surv(days_to_death, vital_status) ~ gistict_EZH2 +
          log2(RNASeq2GeneNorm_EZH2) + pathology_N_stage,  data=wideacc)

## -----------------------------------------------------------------------------
sessionInfo()

