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