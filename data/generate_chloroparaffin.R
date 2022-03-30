min_C <- 0
max_C <- 36
min_Cl <- 0
max_Cl <- 30
chemical_type <- c('CPs', 'COs', 'CdiOs')

forms <- NULL
for(i in 1:length(chemical_type)) {
    forms2 <- expand.grid(min_C:max_C, min_Cl:max_Cl)
    forms2 <- cbind(
        forms2,
        H = if(chemical_type[i]=='CPs') 2 * forms2[, 1] + 2 - forms2[, 2]
            else if (chemical_type[i]=='COs') 2 * forms2[, 1] - forms2[, 2]
            else 2 * forms2[, 1] - 2 - forms2[, 2],
        formula = paste0("C", forms2[, 1], "Cl", forms2[, 2], "H", forms2[, 3]),
        chemical_type = chemical_type[i]
    )
    forms2 <- forms2[which(forms2$H > 0), ]
    forms2$formula <- enviPat::check_chemform(
        isotopes,
        forms2$formula
    )$new_formula
    colnames(forms2) <- c("C", "Cl", "H", "formula", "chemical_type")
    forms <- rbind(forms, forms2)
}

forms <- rbind(
    forms,
    data.frame(
        C = 0,
        Cl = 0,
        H = 0,
        formula = c('C12[2]H18Br6', '[13]C12H18Br6'),
        chemical_type = "standard"
    )
)

forms <- cbind(chemical = seq(nrow(forms)), forms)
write.csv(forms, "data/chemical.csv", row.names = FALSE)
