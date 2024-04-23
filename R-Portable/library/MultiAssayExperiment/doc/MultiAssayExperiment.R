## ----eval = FALSE-------------------------------------------------------------
#  if (!require("BiocManager"))
#      install.packages("BiocManager")
#  BiocManager::install("MultiAssayExperiment")

## ----include=TRUE,results="hide",message=FALSE,warning=FALSE------------------
library(MultiAssayExperiment)
library(GenomicRanges)
library(SummarizedExperiment)
library(RaggedExperiment)

## -----------------------------------------------------------------------------
empty <- MultiAssayExperiment()
empty
slotNames(empty)

## ----echo = FALSE, fig.cap = "MultiAssayExperiment object schematic shows the design of the infrastructure class. The colData provides data about the patients, cell lines, or other biological units, with one row per unit and one column per variable. The experiments are a list of assay datasets of arbitrary class, with one column per observation. The sampleMap links a single table of patient data (colData) to a list of experiments via a simple but powerful table of experiment:patient edges (relationships), that can be created automatically in simple cases or in a spreadsheet if assay-specific sample identifiers are used. sampleMap relates each column (observation) in the assays (experiments) to exactly one row (biological unit) in colData; however, one row of colData may map to zero, one, or more columns per assay, allowing for missing and replicate assays. Green stripes indicate a mapping of one subject to multiple observations across experiments.", out.width = "\\maxwidth"----
knitr::include_graphics("MultiAssayExperiment.png")

## -----------------------------------------------------------------------------
class(experiments(empty)) # ExperimentList

## -----------------------------------------------------------------------------
patient.data <- data.frame(sex=c("M", "F", "M", "F"),
    age=38:41,
    row.names=c("Jack", "Jill", "Bob", "Barbara"))
patient.data

## -----------------------------------------------------------------------------
is(sampleMap(empty), "DataFrame") # TRUE

## -----------------------------------------------------------------------------
exprss1 <- matrix(rnorm(16), ncol = 4,
        dimnames = list(sprintf("ENST00000%i", sample(288754:290000, 4)),
                c("Jack", "Jill", "Bob", "Bobby")))
exprss2 <- matrix(rnorm(12), ncol = 3,
        dimnames = list(sprintf("ENST00000%i", sample(288754:290000, 4)),
                c("Jack", "Jane", "Bob")))
doubleExp <- list("methyl 2k"  = exprss1, "methyl 3k" = exprss2)
simpleMultiAssay <- MultiAssayExperiment(experiments=doubleExp)
simpleMultiAssay

## -----------------------------------------------------------------------------
colData(simpleMultiAssay)

## -----------------------------------------------------------------------------
simpleMultiAssay2 <- MultiAssayExperiment(experiments=doubleExp,
                                          colData=patient.data)
simpleMultiAssay2
colData(simpleMultiAssay2)

## -----------------------------------------------------------------------------
class(metadata(empty)) # NULL (class "ANY")

## -----------------------------------------------------------------------------
metadata(experiments(empty))

## ----message=FALSE------------------------------------------------------------
(arraydat <- matrix(seq(101, 108), ncol=4,
    dimnames=list(c("ENST00000294241", "ENST00000355076"),
    c("array1", "array2", "array3", "array4"))))

coldat <- data.frame(slope53=rnorm(4),
    row.names=c("array1", "array2", "array3", "array4"))

exprdat <- SummarizedExperiment(arraydat, colData=coldat)
exprdat

## -----------------------------------------------------------------------------
(exprmap <- data.frame(primary=rownames(patient.data)[c(1, 2, 4, 3)],
                       colname=c("array1", "array2", "array3", "array4"),
                       stringsAsFactors = FALSE))

## -----------------------------------------------------------------------------
(methyldat <-
   matrix(1:10, ncol=5,
          dimnames=list(c("ENST00000355076", "ENST00000383706"),
                        c("methyl1", "methyl2", "methyl3",
                          "methyl4", "methyl5"))))

## -----------------------------------------------------------------------------
(methylmap <- data.frame(primary = c("Jack", "Jack", "Jill", "Barbara", "Bob"),
    colname = c("methyl1", "methyl2", "methyl3", "methyl4", "methyl5"),
    stringsAsFactors = FALSE))

## -----------------------------------------------------------------------------
(microdat <- matrix(201:212, ncol=3,
                    dimnames=list(c("hsa-miR-21", "hsa-miR-191",
                                    "hsa-miR-148a", "hsa-miR148b"),
                                  c("micro1", "micro2", "micro3"))))

## -----------------------------------------------------------------------------
(micromap <- data.frame(primary = c("Jack", "Barbara", "Bob"),
    colname = c("micro1", "micro2", "micro3"), stringsAsFactors = FALSE))

## -----------------------------------------------------------------------------
nrows <- 5; ncols <- 4
counts <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
rowRanges <- GRanges(rep(c("chr1", "chr2"), c(2, nrows - 2)),
    IRanges(floor(runif(nrows, 1e5, 1e6)), width=100),
    strand=sample(c("+", "-"), nrows, TRUE),
    feature_id=sprintf("ID\\%03d", 1:nrows))
names(rowRanges) <- letters[1:5]
colData <- DataFrame(Treatment=rep(c("ChIP", "Input"), 2),
    row.names= c("mysnparray1", "mysnparray2", "mysnparray3", "mysnparray4"))
rse <- SummarizedExperiment(assays=SimpleList(counts=counts),
    rowRanges=rowRanges, colData=colData)

## -----------------------------------------------------------------------------
(rangemap <-
    data.frame(primary = c("Jack", "Jill", "Bob", "Barbara"),
    colname = c("mysnparray1", "mysnparray2", "mysnparray3", "mysnparray4"),
        stringsAsFactors = FALSE))

## -----------------------------------------------------------------------------
listmap <- list(exprmap, methylmap, micromap, rangemap)
names(listmap) <- c("Affy", "Methyl 450k", "Mirna", "CNV gistic")
listmap

## -----------------------------------------------------------------------------
dfmap <- listToMap(listmap)
dfmap

## ----eval=FALSE---------------------------------------------------------------
#  mapToList(dfmap, "assay")

## -----------------------------------------------------------------------------
objlist <- list("Affy" = exprdat, "Methyl 450k" = methyldat,
    "Mirna" = microdat, "CNV gistic" = rse)

## -----------------------------------------------------------------------------
myMultiAssay <- MultiAssayExperiment(objlist, patient.data, dfmap)
myMultiAssay

## -----------------------------------------------------------------------------
experiments(myMultiAssay)
colData(myMultiAssay)
sampleMap(myMultiAssay)
metadata(myMultiAssay)

## -----------------------------------------------------------------------------
objlist3 <- objlist
(names(objlist3) <- NULL)

try(prepMultiAssay(objlist3, patient.data, dfmap)$experiments,
    outFile = stdout())

## -----------------------------------------------------------------------------
names(objlist3) <- toupper(names(objlist))
names(objlist3)
unique(dfmap[, "assay"])
prepMultiAssay(objlist3, patient.data, dfmap)$experiments

## -----------------------------------------------------------------------------
exampleMap <- sampleMap(simpleMultiAssay2)
sapply(doubleExp, colnames)
exampleMap
prepMultiAssay(doubleExp, patient.data, exampleMap)$metadata$drops

## -----------------------------------------------------------------------------
exMap <- rbind(dfmap,
    DataFrame(assay = "New methyl", primary = "Joe",
        colname = "Joe"))
invisible(prepMultiAssay(objlist, patient.data, exMap))

## -----------------------------------------------------------------------------
prepped <- prepMultiAssay(objlist, patient.data, exMap)
preppedMulti <- MultiAssayExperiment(prepped$experiments, prepped$colData,
    prepped$sampleMap, prepped$metadata)
preppedMulti

## -----------------------------------------------------------------------------
do.call(MultiAssayExperiment, prepped)

## -----------------------------------------------------------------------------
grlls <- list(chr = rep("chr1", nrows), start = seq(11, 15),
    end = seq(12, 16), strand = c("+", "-", "+", "*", "*"),
    score = seq(1, 5), specimen = c("a", "a", "b", "b", "c"),
    gene_symbols = paste0("GENE", letters[seq_len(nrows)]))

grldf <- as.data.frame(grlls, stringsAsFactors = FALSE)

GRL <- makeGRangesListFromDataFrame(grldf, split.field = "specimen",
    names.field = "gene_symbols")

## -----------------------------------------------------------------------------
RaggedExperiment(GRL)

## -----------------------------------------------------------------------------
sels <- list(chr = rep("chr2", nrows), start = seq(11, 15),
    end = seq(12, 16), strand = c("+", "-", "+", "*", "*"),
    expr0 = seq(3, 7), expr1 = seq(8, 12), expr2 = seq(12, 16))
sedf <- as.data.frame(sels,
    row.names = paste0("GENE", letters[rev(seq_len(nrows))]),
    stringsAsFactors = FALSE)
sedf
makeSummarizedExperimentFromDataFrame(sedf)

## ----eval=FALSE---------------------------------------------------------------
#  myMultiAssay[rows, columns, assays]

## -----------------------------------------------------------------------------
myMultiAssay["ENST00000355076", , ]

## -----------------------------------------------------------------------------
myMultiAssay["ENST00000355076", 1:2, c("Affy", "Methyl 450k")]

## -----------------------------------------------------------------------------
myMultiAssay[, "Jack", ]
myMultiAssay[, 1, ]
myMultiAssay[, c(TRUE, FALSE, FALSE, FALSE), ]

## -----------------------------------------------------------------------------
myMultiAssay[, , "Mirna"]
myMultiAssay[, , 3]
myMultiAssay[, , c(FALSE, FALSE, TRUE, FALSE, FALSE)]

## -----------------------------------------------------------------------------
myMultiAssay["ENST00000355076", , , drop=FALSE]

## -----------------------------------------------------------------------------
myMultiAssay["ENST00000355076", , , drop=TRUE]

## -----------------------------------------------------------------------------
colData(myMultiAssay)

## -----------------------------------------------------------------------------
myMultiAssay[, 1:2]

## -----------------------------------------------------------------------------
malesMultiAssay <- myMultiAssay[, myMultiAssay$sex == "M"]
colData(malesMultiAssay)

## -----------------------------------------------------------------------------
allsamples <- colnames(myMultiAssay)
allsamples

## -----------------------------------------------------------------------------
allsamples[["Methyl 450k"]] <- allsamples[["Methyl 450k"]][-3:-5]
myMultiAssay[, as.list(allsamples), ]
subsetByColumn(myMultiAssay,  as.list(allsamples))  #equivalent

## -----------------------------------------------------------------------------
myMultiAssay[, , c("Affy", "CNV gistic")]

## -----------------------------------------------------------------------------
is.cnv <- grepl("CNV", names(experiments(myMultiAssay)))
is.cnv
myMultiAssay[, , is.cnv]  #logical subsetting
myMultiAssay[, , which(is.cnv)] #integer subsetting

## -----------------------------------------------------------------------------
myMultiAssay[list(Mirna = 1:2), , ]
## equivalently
subsetByRow(myMultiAssay, list(Mirna = 1:2))

## -----------------------------------------------------------------------------
featSub0 <- subsetByRow(myMultiAssay, "ENST00000355076")
featSub1 <- myMultiAssay["ENST00000355076", , drop = FALSE] #equivalent
all.equal(featSub0, featSub1)
class(featSub1)
class(experiments(featSub1))
experiments(featSub1)

## -----------------------------------------------------------------------------
featSubsetted <-
  subsetByRow(myMultiAssay, c("ENST00000355076", "ENST00000294241"))
assay(myMultiAssay, 1L)
assay(featSubsetted, 1L)

## -----------------------------------------------------------------------------
gr <- GRanges(seqnames = c("chr1", "chr1", "chr2"), strand = c("-", "+", "+"),
              ranges = IRanges(start = c(230602, 443625, 934533),
                               end = c(330701, 443724, 934632)))

## -----------------------------------------------------------------------------
subsetted <- subsetByRow(myMultiAssay, gr, maxgap = 2L, type = "within")
experiments(subsetted)
rowRanges(subsetted[[4]])

## -----------------------------------------------------------------------------
names(myMultiAssay)
myMultiAssay[[1]]
myMultiAssay[["Affy"]]

## -----------------------------------------------------------------------------
colData(myMultiAssay)

## -----------------------------------------------------------------------------
complete.cases(myMultiAssay)

## -----------------------------------------------------------------------------
complete.cases(myMultiAssay[, , 1:2])

## -----------------------------------------------------------------------------
myMultiAssay[, complete.cases(myMultiAssay), ]

## -----------------------------------------------------------------------------
replicated(myMultiAssay)

## -----------------------------------------------------------------------------
(ensmblMatches <- intersectRows(myMultiAssay[, , 1:2]))
rownames(ensmblMatches)

## -----------------------------------------------------------------------------
intersectColumns(myMultiAssay)

## -----------------------------------------------------------------------------
mergeReplicates(intersectColumns(myMultiAssay))

## -----------------------------------------------------------------------------
c(myMultiAssay, ExpScores = matrix(1:8, ncol = 4,
dim = list(c("ENSMBL0001", "ENSMBL0002"), paste0("pt", 1:4))),
mapFrom = 1L)

## -----------------------------------------------------------------------------
(affex <- getWithColData(myMultiAssay, 1L))
colData(affex)
class(affex)

## -----------------------------------------------------------------------------
longFormat(myMultiAssay[, , 1:2])

## -----------------------------------------------------------------------------
longFormat(myMultiAssay[, , 1:2], colDataCols="age")

## -----------------------------------------------------------------------------
maemerge <- mergeReplicates(intersectColumns(myMultiAssay))
wideFormat(maemerge, colDataCols="sex")[, 1:5]

## -----------------------------------------------------------------------------
assay(myMultiAssay)

## -----------------------------------------------------------------------------
assays(myMultiAssay)

## ----eval = FALSE-------------------------------------------------------------
#  BiocManager::install("curatedTCGAData")

## -----------------------------------------------------------------------------
rownames(myMultiAssay)
colnames(myMultiAssay)

## ----error = TRUE-------------------------------------------------------------
objlist2 <- objlist
objlist2[[2]] <- as.vector(objlist2[[2]])

tryCatch(
    MultiAssayExperiment(objlist2, patient.data, dfmap),
    error = function(e) {
        conditionMessage(e)
    }
)

## -----------------------------------------------------------------------------
methods(class="MultiAssayExperiment")

## -----------------------------------------------------------------------------
citation("MultiAssayExperiment")

## -----------------------------------------------------------------------------
sessionInfo()

