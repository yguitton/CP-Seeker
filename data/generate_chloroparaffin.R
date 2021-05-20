min_C <- 7
max_C <- 36
min_Cl <- 3
max_Cl <- 30
adduct_names <- c('M+Cl', 'M-H', 'M+Hac-H')
chemical_type <- c('CPs', 'COs', 'CdiOs')

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

forms <- NULL
for(i in 1:length(chemical_type)){
  forms2 <- NULL
  forms2 <- rbind(forms2, expand.grid(min_C:max_C, min_Cl:max_Cl))
  if(chemical_type[i]=='CPs') forms2 <- cbind(forms2, Var3 = 2 * forms2[, 1] + 2 - forms2[, 2])
  else if (chemical_type[i]=='COs') forms2 <- cbind(forms2, Var3 = 2 * forms2[, 1] - forms2[, 2])
  else if (chemical_type[i]=='CdiOs') forms2 <- cbind(forms2, Var3 = 2 * forms2[, 1] - 2 - forms2[, 2])
  forms2 <- forms2[which(forms2[, 3] > 0), ]
  forms2 <- cbind(forms2, Var4 = paste("C", forms2[, 1], "Cl", forms2[, 2], 
	  "H", forms2[, 3], sep = ""))
  forms2[, "Var4"] <- enviPat::check_chemform(isotopes, forms2[, "Var4"])$new_formula
  forms2 <- cbind(forms2, var5 = chemical_type[i])
  colnames(forms2) <- c("C", "Cl", "H", "formula", "chemical_type")
  forms <- rbind(forms, forms2)
}
forms <- cbind(chemical = seq(nrow(forms)), forms)
write.csv(forms, "~/GitHub/CP-Seeker/data/chemical.csv", row.names = FALSE)

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
		chemical = forms[, "chemical"],
		chemical_type = forms[, "chemical_type"],
		stringsAsFactors = FALSE
	)
))
ion_forms <- cbind(chemical_ion = seq(nrow(ion_forms)), ion_forms)
write.csv(ion_forms, "~/GitHub/CP-Seeker/data/chemical_ion.csv", row.names = FALSE)