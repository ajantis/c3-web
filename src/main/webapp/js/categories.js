$(document).ready(function(){
    //current category
    $(".tagAddButton").live("click",function(){
         var CatName = $(this).attr("id");
         $(".input_categoty").val(CatName);
    });
    //for delete, current value tag
    $(".close_orient").live("click",function(){
        var tag = $(this).parent().children("span").text();
        $(this).parent().children(".tagName").val(tag);
        var CatName = $(this).parent().parent().children("h2").text();
        $(this).parent().children(".CatName").val(CatName);
    });

});