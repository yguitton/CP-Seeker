# Script to create the chemical_ions file from chemical file
# Be careful chemical file needs to be in CSV with "," separating columns !!
# AND formula needs number 1 when there is only one atom !!

forms <- read.csv("data/chemical.csv")
# List of wanted adducts
adduct_names <- c('M+Cl', 'M-H', 'M+Hac-H', 'M+Br', 'M-Cl', 'M-HCl', 'M-Br',
                  'M-HBr')
standard_adduct_names <- c(adduct_names, 'M-D')
# List of ECNI or ESI/APCI
# Please add all adducts from adduct_names and standard_adduct_names here
ecni_list <- c('M-Cl', 'M-HCl', 'M-Br', 'M-HBr')
esi_list <- c('M+Cl', 'M-H', 'M+Hac-H', 'M+Br', 'M-D')


get_ions <- function(forms, adduct) {
  default_df <- data.frame(matrix(, nrow = 0, ncol = 4, dimnames = list(c(),
        c("formula", "adduct", "ion_formula", "charge"))))
  ion_forms <- forms
    if (adduct$Mult > 1) {
        ion_forms <- enviPat::multiform(ion_forms, adduct$Mult)
    }
    if (adduct$Formula_add != "FALSE") {
        ion_forms <- enviPat::mergeform(ion_forms, adduct$Formula_add)
    }
    if (adduct$Formula_ded != "FALSE") {
        test <- enviPat::check_ded(ion_forms, adduct$Formula_ded)
        if (any(test == FALSE)) {
            forms <- forms[test == FALSE]
            ion_forms <- enviPat::subform(
                ion_forms[test == FALSE],
                adduct$Formula_ded
            )
        } else {
            return(default_df)
        }
    }
    data.frame(
        ion_formula = ion_forms,
        adduct = adduct$Name,
        charge = adduct$Charge,
        formula = forms
    )
}

data("isotopes", package = "enviPat")
data("adducts", package = "enviPat")
# add adducts asked by Manue & Ronan
adducts <- rbind(
    adducts,
    data.frame(
    	Name = c("M-Cl", "M-HCl", "M-Br", "M-HBr", "M-D"),
    	calc = c("M-34.9683", "M-35.97613", "M-78.91779", "M-79.92507", "M-2.013553"),
    	Charge = c(-1, -1, -1, -1, -1),
    	Mult = c(1, 1, 1, 1, 1),
    	Mass = c(-34.9683, -35.97613, -78.91779, -79.92507, -2.013553),
    	Ion_mode = c("negative", "negative", "negative", "negative", "negative"),
    	Formula_add = c("FALSE", "FALSE", "FALSE", "FALSE", "FALSE"),
    	Formula_ded = c("Cl1", "H1Cl1", "Br1", "H1Br1", "D1"),
    	Multi = c(1, 1, 1, 1, 1)
    )
)
adducts <- adducts[which(adducts[, "Name"] %in% adduct_names), ]
# order isotopes to have first carbons, then hydrogens, then elements in alphabetical order
elts_CH <- unlist(lapply(c("C", "[12]C", "[13]C", "H", "D", "[1]H", "[2]H"), function(elt)
	which(isotopes$element == elt)))
isotopes_CH <- isotopes[elts_CH, ]
isotopes_not_CH <- isotopes[-elts_CH, ]
isotopes <- rbind(isotopes_CH, isotopes_not_CH[order(
	isotopes_not_CH$element), ])

standard <- forms[which(forms$chemical_familly == "Standard"), ]
forms <- forms[which(forms$chemical_familly != "Standard"), ]
ion_forms <- do.call(rbind, lapply(seq(nrow(adducts)), function(i){
	print(adducts[i,"Name"]);
  get_ions(unique(forms$formula), adducts[i, ])
}))
ion_forms <- merge(forms, ion_forms, by = "formula", all.x = TRUE)
ion_forms <- ion_forms[, c("ion_formula", "adduct", "charge", "chemical",
                           "chemical_type")]

standard_adducts <- adducts[which(adducts[, "Name"] %in% standard_adduct_names), ]
ion_standard <- do.call(rbind, lapply(seq(nrow(standard_adducts)), function(i)
    get_ions(standard$formula, standard_adducts[i, ])))
ion_standard[which(ion_standard$adduct == "M-D"), "adduct"] <- "M-H"
ion_standard <- merge(standard, ion_standard, by = "formula", all.x = TRUE)
ion_standard <- ion_standard[, c("ion_formula", "adduct", "charge", "chemical",
                           "chemical_type")]

ion_forms <- rbind(ion_forms, ion_standard)

ion_forms <- cbind(chemical_ion = seq(nrow(ion_forms)), ion_forms)

ion_family <- rep("", nrow(ion_forms))
ion_family[which(ion_forms$adduct %in% ecni_list)] <- "ECNI"
ion_family[which(ion_forms$adduct %in% esi_list)] <- "ESI/APCI"
ion_forms <- cbind(ion_forms, chemical_ion_family = ion_family)
write.csv(ion_forms, "data/chemical_ion.csv", row.names = FALSE)
