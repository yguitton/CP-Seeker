$(function(){
	
});
 
function addSelectItem(t,ev)
{
   ev.stopPropagation();
   
   var bs = $(t).closest('.bootstrap-select')
   var txt=bs.find('.bss-input').val().replace(/[|]/g,"");
   var txt=$(t).prev().val().replace(/[|]/g,"");
   if ($.trim(txt)=='') return null;
   
   // Changed from previous version to cater to new
   // layout used by bootstrap-select.
   var p=bs.find('select');
   var o=$('option', p).eq(-2);
   o.before( $("<option>", { "selected": false, "text": txt}) );
   p.selectpicker('refresh');
}
 
function addSelectInpKeyPress(t,ev)
{
   ev.stopPropagation();
 
   // do not allow pipe character
   if (ev.which==124) ev.preventDefault();
 
   // enter character adds the option
   if (ev.which==13)
   {
      ev.preventDefault();
      addSelectItem($(t).next(),ev);
   }
}