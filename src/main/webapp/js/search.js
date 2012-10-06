function searchClick(){
    if (event.keyCode == 13) document.getElementById('btnSearch').click()
}
$(document).ready( function(){
    $('.search-query').focus(function() {
        $(this).stop().animate({width: '400px'}, 'slow');
    });
    $('.search-query').blur(function() {
        $(this).stop().animate({width: '200px'},'fast');
    });
});