 $(document).ready(function(){
    //show/hide add category
    $("#category_name_1").click(function(){
        $("#category_name_2").toggleClass("category_name_hide category_name_show");

    });
    //show/hide add tags
     $(".tagAddButton").live("click",function(){
            $(".tagNameForm").toggleClass("category_name_hide category_name_show");

     });
});