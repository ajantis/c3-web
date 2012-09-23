 $(document).ready(function(){
    //hover categories
    $('.category_cont').hover(function(){
    $('.category_tags').css("display","none");
    $(this).next().css("display","block");
    });
    $('.category_tags').hover(
        function(){},
       function(){
        $(this).css("display","none");
        });
    });