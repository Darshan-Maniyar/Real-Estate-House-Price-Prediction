$(document).ready(function()){
//initial
$('#content').load('content/index.html');
//handle menuclick
$('ul#nav li a').click(function()){
	var page = $(this).attr('href');
	
	$('#content').load('content/'+ page + ',html');
});

});