## ----style, echo = FALSE, results = 'asis', message=FALSE---------------------
BiocStyle::markdown()

## ----supportedFilters---------------------------------------------------------
library(AnnotationFilter)
supportedFilters()

## ----symbol-filter------------------------------------------------------------
library(AnnotationFilter)

smbl <- SymbolFilter("BCL2")
smbl

## ----symbol-startsWith--------------------------------------------------------
smbl <- SymbolFilter("BCL2", condition = "startsWith")
smbl

## ----convert-expression-------------------------------------------------------
smbl <- AnnotationFilter(~ symbol == "BCL2")
smbl

## ----convert-multi-expression-------------------------------------------------
flt <- AnnotationFilter(~ symbol == "BCL2" &
                            tx_biotype == "protein_coding")
flt

## ----nested-query-------------------------------------------------------------
## Define the filter query for the first pair of filters.
afl1 <- AnnotationFilterList(SymbolFilter("BCL2L11"),
                             TxBiotypeFilter("nonsense_mediated_decay"))
## Define the second filter pair in ( brackets should be combined.
afl2 <- AnnotationFilterList(SymbolFilter("BCL2"),
                             TxBiotypeFilter("protein_coding"))
## Now combine both with a logical OR
afl <- AnnotationFilterList(afl1, afl2, logicOp = "|")

afl

## ----define-data.frame--------------------------------------------------------
## Define a simple gene table
gene <- data.frame(gene_id = 1:10,
                   symbol = c(letters[1:9], "b"),
                   seq_name = paste0("chr", c(1, 4, 4, 8, 1, 2, 5, 3, "X", 4)),
                   stringsAsFactors = FALSE)
gene

## ----simple-symbol------------------------------------------------------------
smbl <- SymbolFilter("b")

## ----simple-symbol-condition--------------------------------------------------
condition(smbl)

## ----simple-symbol-value------------------------------------------------------
value(smbl)

## ----simple-symbol-field------------------------------------------------------
field(smbl)

## ----doMatch------------------------------------------------------------------

doMatch <- function(x, filter) {
    do.call(condition(filter), list(x[, field(filter)], value(filter)))
}

## Apply this function
doMatch(gene, smbl)


## ----doExtract----------------------------------------------------------------

doExtract <- function(x, filter) {
    x[doMatch(x, filter), ]
}

## Apply it on the data
doExtract(gene, smbl)

## ----doMatch-formula----------------------------------------------------------

doMatch <- function(x, filter) {
    if (is(filter, "formula"))
        filter <- AnnotationFilter(filter)
    do.call(condition(filter), list(x[, field(filter)], value(filter)))
}

doExtract(gene, ~ gene_id == '2')


## ----orgDb, message = FALSE---------------------------------------------------
## Load the required packages
library(org.Hs.eg.db)
library(RSQLite)
## Get the database connection
dbcon <- org.Hs.eg_dbconn()

## What tables do we have?
dbListTables(dbcon)

## ----gene_info----------------------------------------------------------------
## What fields are there in the gene_info table?
dbListFields(dbcon, "gene_info")

## ----doExtractSQL-------------------------------------------------------------

doExtractGene <- function(x, filter) {
    gene <- dbGetQuery(x, "select * from gene_info")
    doExtract(gene, filter)
}

## Extract all entries for BCL2
bcl2 <- doExtractGene(dbcon, SymbolFilter("BCL2"))

bcl2

## ----simpleSQL----------------------------------------------------------------

## Define a simple function that covers some condition conversion
conditionForSQL <- function(x) {
    switch(x,
           "==" = "=",
           x)
}

## Define a function to translate a filter into an SQL where condition.
## Character values have to be quoted.
where <- function(x) {
    if (is(x, "CharacterFilter"))
        value <- paste0("'", value(x), "'")
    else value <- value(x)
    paste0(field(x), conditionForSQL(condition(x)), value)
}

## Now "translate" a filter using this function
where(SeqNameFilter("Y"))


## ----doExtractGene2-----------------------------------------------------------

## Define a function that 
doExtractGene2 <- function(x, filter) {
    if (is(filter, "formula"))
        filter <- AnnotationFilter(filter)
    query <- paste0("select * from gene_info where ", where(filter))
    dbGetQuery(x, query)
}

bcl2 <- doExtractGene2(dbcon, ~ symbol == "BCL2")
bcl2


## ----performance--------------------------------------------------------------
system.time(doExtractGene(dbcon, ~ symbol == "BCL2"))

system.time(doExtractGene2(dbcon, ~ symbol == "BCL2"))


## ----symbol-overwrite---------------------------------------------------------
## Default method from AnnotationFilter:
field(SymbolFilter("a"))

## Overwrite the default method.
setMethod("field", "SymbolFilter", function(object, ...) "hgnc_symbol")

## Call to field returns now the "correct" database column
field(SymbolFilter("a"))


## ----si-----------------------------------------------------------------------
sessionInfo()

