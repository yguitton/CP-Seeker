 var dimension = [0, 0];
 var df;
$(document).on("shiny:connected", function(e) {
	$("body").addClass("sidebar-mini");
	Shiny.onInputChange("dimension", [window.innerWidth, window.innerHeight]);
	$('.dropdown-menu').css('width', window.innerWidth/4.8);
	toastr.options = {"positionClass": "toast-top-center"};
	$('#download').removeClass('btn btn-default');
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

$(document).on('change', '#fileImportRaw', function(){
	Shiny.onInputChange('sampleIDTable_final', table.data().toArray());
});

$(document).on('change', '#fileImportImportmzXML', function(){
	Shiny.onInputChange('sampleIDTableXML_final', table.data().toArray());
});

$(document).on('click', '#fileDBAdd2', function(){
	var vals = $('#sampleIDDBTable input').toArray().map(x => x.value);
	console.log(vals);
	if(vals.some(x => x.length > 30)){
		toastr.error('sampleID cannot contain more than 30 characters', '', 
			{positionClass: 'toast-top-center',
			closeButton: true, 
			newestOnTop: true, 
			preventDuplicates: true
		})
	}
	else {
		Shiny.onInputChange('sampleIDsDBValues', vals);
		Shiny.onInputChange('sampleIDsDBValidBttn', Math.random());
	}
})

$(document).on('click', '#fileAdd', function(){
	var vals = $('#sampleIDTable input').toArray().map(x => x.value);
	console.log(vals);
	if(vals.some(x => x.length > 30)){
		toastr.error('sampleID cannot contain more than 30 characters', '', 
			{positionClass: 'toast-top-center',
			closeButton: true, 
			newestOnTop: true, 
			preventDuplicates: true
		})
	}
	else {
		Shiny.onInputChange('sampleIDsValues', vals);
		Shiny.onInputChange('sampleIDsValidBttn', Math.random());
	}
})

$(document).on('change', '#detailsSwitch2 input', function(){
	console.log($('#detailsSwitch2 input').toArray().filter(x => x.checked)[0].value);
})

Shiny.addCustomMessageHandler("dfUpdate", function(message){
	df = message;	
})
