function searchClick(){
    if (event.keyCode == 13) document.getElementById('btnSearch').click()
}
function SelectAll(sel){
    var SelectedOptions = document.getElementById(sel);
    var msg="";
    for (var i=0;i<SelectedOptions.options.length;i++){
        if(SelectedOptions.options[i].selected)
            msg +=SelectedOptions.options[i].value+'%';
    }
    //alert(msg);
    return msg;

}
$(document).ready( function(){
     //add metadata
    $(".btn_add_metadata").live("click",function(){
        var keyExist = true;
        var key = $("#key").val();
        var value = $("#value").val();
        key = $.trim(key);
        value = $.trim(value);
        $(".metadata_key").each(function(index,el){
            if(el.value==key) keyExist = false;
        });
        if(key!="" && value!="" && keyExist){
            $("#key").val("");
            $("#value").val("");
            $(".metadata_form").append('<tr>'+
                                            '<td><input readonly class="metadata_key" value="'+ key + '"/></td>'+
                                            '<td><input class="metadata_value" value="'+ value + '"/></td>'+
                                            '<td><a class="close remove_metadata">&times;</a></td>'+
                                       '</tr>');

        }
        if(!keyExist){
            alert("This key is exists");
        }

    });
    //select metadata from table
     $(".btn_save_metadata").live("click",function(){
        var key = ""
        var value = ""
        $(".metadata_key").each(function(index,el){
            key += el.value +"%"

        });
        $(".metadata_value").each(function(index,el){
                    value += el.value +"%"
        });
        $("#key_cont").val(key);
        $("#value_cont").val(value);
     });
     $(".remove_metadata").live("click",function(){
        $(this).parent().parent().remove();

     });
     $('.btn_add_user').live("click",function(){
        $('#listusersadd').val(SelectAll("selusersadd"));
    });

    $('.buttonuser1').live("click",function(){
        $('#listusers').val(SelectAll("selusers"));
    });

    //$('.search-query').focus(function() {
    //    $(this).stop().animate({width: '400px'}, 'slow');
    //});
    //$('.search-query').blur(function() {
    //    $(this).stop().animate({width: '200px'},'fast');
    //});
    // add tag in search
    $(".margn").live("click",function(){
       $(this).appendTo(".flt");
       $(this).removeClass();
       $(this).wrap('<div class="label-info margn_search"></div>');
       $(this).after('<a class="close cls">&times;</a>');

    });
    // delete tag from search
    $(".cls").live("click",function(){
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
     $('.con_category').hover(function(){
        $('.category_tags').css("display","none");
        $(this).next().css("display","block");
     });
     $('.category_tags').hover(
        function(){},
        function(){
        $(this).css("display","none");
     });
});