 $(document).ready(function(){
    $("#category_name_1").click(function(){
        $("#category_name_2").toggleClass("category_name_hide category_name_show");

    });
     $(".tagAddButton").live("click",function(){
            $(".tagNameForm").toggleClass("category_name_hide category_name_show");

        });

    $(".margn").live("click",function(){
       $(this).appendTo(".flt");
       $(this).removeClass();
       $(this).wrap('<div class="label-info margn_search"></div>');
       $(this).after('<a class="close cls">&times;</a>');

    });
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
    $(".tabs-left1").live("click",function(){
        var CatName = $(".tabs-left1.active").children("a").text();
        $(".inputCateg").val(CatName);
    });

 });