 var dimension = [0, 0];
$(document).on("shiny:connected", function(e) {
	dimension[0] = window.innerWidth;
	dimension[1] = window.innerHeight;
	Shiny.onInputChange("dimension", dimension);
	Shiny.onInputChange('targetTable_cell_selected', {C:0, Cl:0});
});