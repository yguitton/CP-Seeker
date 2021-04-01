min_C <- 7
max_C <- 36
min_Cl <- 3
max_Cl <- 30
adduct_names <- c('M+Cl', 'M-H', 'M+Hac-H')

#' @title Merge two formulas
#'
#' @description
#' Merge two formulas. Adapt from enviPat::mergeform to manage "FALSE" values
#'
#' @param forms vector(string) chemical formulas
#' @param form string chemical formula to add
#'
#' @return vector(string) chemical formulas merged
#' 
#' @examples
#' \dontrun{mergeform("C12H18Br6", "H1")}
mergeform <- function(forms, form) if (form == "FALSE") forms else enviPat::mergeform(forms, form)

#' @title Deduct a formula to others
#'
#' @description
#' Deduct a formula to others. Adapt from enviPat::mergeform to manage "FALSE" values
#'
#' @param forms vector(string) chemical formulas
#' @param form string chemical formula to deduct
#'
#' @return vector(string) chemical formulas deducted
#' 
#' @examples
#' \dontrun{subform("C12H19Br6", "H1")}
subform <- function(forms, form) {
    if (form == "FALSE") forms
    else {
        test <- enviPat::check_ded(forms, form)
        if (any(!test)) enviPat::subform(forms[which(!test)], form)
        else NULL
    }
}

data("isotopes", package = "enviPat")
data("adducts", package = "enviPat")
adducts <- adducts[which(adducts[, "Name"] %in% adduct_names), ]
# order isotopes to have first carbons, then hydrogens, then elements in alphabetical order
elts_CH <- unlist(lapply(c("C", "[12]C", "[13]C", "H", "D", "[1]H", "[2]H"), function(elt)
	which(isotopes$element == elt)))
isotopes_CH <- isotopes[elts_CH, ]
isotopes_not_CH <- isotopes[-elts_CH, ]
isotopes <- rbind(isotopes_CH, isotopes_not_CH[order(
	isotopes_not_CH$element), ])

forms <- expand.grid(min_C:max_C, min_Cl:max_Cl)
forms <- cbind(forms, Var3 = 2 * forms[, 1] + 2 - forms[, 2])
forms <- forms[which(forms[, 3] > 0), ]
forms <- cbind(forms, Var4 = paste("C", forms[, 1], "Cl", forms[, 2], 
	"H", forms[, 3], sep = ""))
forms[, "Var4"] <- enviPat::check_chemform(isotopes, forms[, "Var4"])$new_formula
colnames(forms) <- c("C", "Cl", "H", "formula")
forms <- cbind(chloroparaffin = seq(nrow(forms)), forms)
write.csv(forms, "~/GitHub/CP-Seeker/data/chloroparaffin.csv", row.names = FALSE)

ion_forms <- do.call(rbind, pbapply::pblapply(seq(nrow(adducts)), function(i) 
	cbind.data.frame(
		ion_formula = subform(
			mergeform(
				enviPat::multiform(
					forms[, "formula"], 
					adducts[i, "Multi"]
				), 
				adducts[i, "Formula_add"]
			), 
			adducts[i, "Formula_ded"]
		), 
		adduct = adducts[i, "Name"], 
		charge = adducts[i, "Charge"], 
		chloroparaffin = forms[, "chloroparaffin"], 
		stringsAsFactors = FALSE
	)
))
ion_forms <- cbind(chloroparaffin_ion = seq(nrow(ion_forms)), ion_forms)
write.csv(ion_forms, "~/GitHub/CP-Seeker/data/chloroparaffin_ion.csv", row.names = FALSE)