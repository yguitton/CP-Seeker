 var dimension = [0, 0];
$(document).on("shiny:connected", function(e) {
	Shiny.onInputChange("dimension", [window.innerWidth, window.innerHeight]);
})

function getWidthSidebar(){
	var sidebar = document.getElementsByClassName('main-sidebar')[0];
	
	if(sidebar.attributes['data-collapsed'].value == "false"){
		return sidebar.clientWidth;
	} else {
		return 0;
	} 
}

$(window).resize(function(){
	dimension[0] = window.innerWidth - getWidthSidebar();
	dimension[1] = window.innerHeight;
	
	Shiny.onInputChange("dimension", dimension);
})
