## ----read_data, message = FALSE-----------------------------------------------
library("QFeatures")
data(hlpsms)
hl <- readQFeatures(hlpsms, ecol = 1:10, name = "psms")

## ----aggregateFeatures--------------------------------------------------------
hl <- aggregateFeatures(hl, "psms", "Sequence", name = "peptides", fun = colMeans)
hl <- aggregateFeatures(hl, "peptides", "ProteinGroupAccessions", name = "proteins", fun = colMeans)

## ----add_TMT_info-------------------------------------------------------------
hl$tag <- c("126", "127N", "127C", "128N", "128C", "129N", "129C",
            "130N", "130C", "131")

## ----plot---------------------------------------------------------------------
plot(hl)

## ----plot2, out.width="400px"-------------------------------------------------
data("feat3")
plot(feat3)

## ----plot_interactive, eval = FALSE-------------------------------------------
#  plot(hl, interactive = TRUE)

## ----plot_assay---------------------------------------------------------------
plot(assay(hl, "proteins")[1, ])

## ----hist_rowData-------------------------------------------------------------
hist(rowData(hl)[["proteins"]]$.n)

## ----table_tag----------------------------------------------------------------
table(hl$tag)

## ----ggplot_rowData, message = FALSE------------------------------------------
library("ggplot2")
df <- data.frame(rowData(hl)[["proteins"]])
ggplot(df) +
    aes(x = .n) +
    geom_histogram()

## ----ComplexHeatmap, message = FALSE------------------------------------------
library(ComplexHeatmap)
Heatmap(matrix = assay(hl, "proteins"),
        show_row_names = FALSE)

## ----ComplexHeatmap_annotations-----------------------------------------------
ha <- rowAnnotation(markers = rowData(hl)[["proteins"]]$markers)
Heatmap(matrix = assay(hl, "proteins"),
        show_row_names = FALSE,
        left_annotation = ha)

## ----longFormat, fig.height = 7, fig.width = 9--------------------------------
lf <- longFormat(hl[, , "proteins"],
                 rowvars = "markers",
                 colvars = "tag")
ggplot(data.frame(lf)) +
    aes(x = tag,
        y = value,
        group = rowname) +
    geom_line() +
    facet_wrap(~ markers, scales = "free_y", ncol = 3)

## ----display, eval = FALSE----------------------------------------------------
#  display(hl)

## ----heatmapdisplay, results='markup', fig.cap="`QFeatures` interactive interface: heatmap of the peptide assay data.", echo=FALSE, out.width='100%', fig.align='center', fig.wide = TRUE----
knitr::include_graphics("./figs/display_hmap.png", error = FALSE)

## ----assaydisplay, results='markup', fig.cap="`QFeatures` interactive interface: quantitative peptide assay data.", echo=FALSE, out.width='100%', fig.align='center', fig.wide = TRUE----
knitr::include_graphics("./figs/display_assay.png", error = FALSE)

## ----rowdatadisplay, results='markup', fig.cap="`QFeatures` interactive interface: peptide assay row data", echo=FALSE, out.width='100%', fig.align='center', fig.wide = TRUE----
knitr::include_graphics("./figs/display_rowdata.png", error = FALSE)

## ----sessioninfo, echo=FALSE--------------------------------------------------
sessionInfo()

