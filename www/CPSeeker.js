var old_matrix;
Shiny.addCustomMessageHandler('matrix', function(mat){
	old_matrix = mat;
});
var new_values;
Shiny.addCustomMessageHandler('values', function(val){
	new_values = val;
});
var regression_mat;
Shiny.addCustomMessageHandler('regression', function(reg){
	regression_mat = reg;
});
 $(document).on("shiny:connected", function(e) {
	$("body").addClass("sidebar-mini");
	$('.dropdown-menu').css('width', window.innerWidth/4.8);
});

// @ description 
	// reinitialize rt & button for Shiny
$(document).on('click', '#eic_draw_tic', function(){
	Shiny.onInputChange('eic_rt', 0);
	Shiny.onInputChange('eic_draw_value', 'TIC');
	Shiny.onInputChange('eic_draw_random', Math.random());
})

// @ description 
	// reinitialize rt & button for Shiny
$(document).on('click', '#eic_draw_eic', function(){
	Shiny.onInputChange('eic_rt', 0);
	Shiny.onInputChange('eic_draw_value', 'EIC');
	Shiny.onInputChange('eic_draw_random', Math.random());
})

// @ description
	// return unique elements in an array
function unique(value, index, self) { 
    return self.indexOf(value) === index;
}