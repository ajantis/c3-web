 $(document).ready(function(){
    //show/hide add category
    $("#category_name_1").click(function(){
        $("#category_name_2").toggleClass("category_name_hide category_name_show");

    });
    //show/hide add tags
     $(".tagAddButton").live("click",function(){
            $(".tagNameForm").toggleClass("category_name_hide category_name_show");

     });
     // add tag in search
    $(".margn").live("click",function(){
       $(this).appendTo(".flt");
       $(this).removeClass();
       $(this).wrap('<div class="label-info margn_search"></div>');
       $(this).after('<a class="close cls">&times;</a>');

    });
    // delete tag from search
    $(".close").live("click",function(){
      var $parent = $(this).parent();
      var id = $parent.children("span").attr("id");
      var id_new = "";
      var i = 0;
      while(true){
          if(id[i]!='_'){
            id_new = id_new + id[i];
          }else{
            break;
          }
      i++;
      }
      $parent.children("span").addClass("label margn");
      $parent.children("span").appendTo("#"+id_new);
      $parent.remove();
    });

     //current category
     $(".tabs-left1").live("click",function(){
         var CatName = $(".tabs-left1.active").children("a").text();
         $(".inputCateg").val(CatName);
     });
     //current pick tags
    $(".btn_search").live("click",function(){
        var tags="";
        var flag = true;
        $(".flt").find("span").each( function () {
            if(flag){
                tags = $(this).text();
                flag = false;
            } else{
                tags = tags+","+$(this).text();
            }
        });
        $(".tags_input").val(tags);
    });

 });