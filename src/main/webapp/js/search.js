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
    // show/hide metadata

     $('.metadata_btn').click(function () {
        $(".test").toggleClass("icon-chevron-up");
        $(".test").toggleClass("icon-chevron-down");
        $(this).parent().children('.edit_metadata').toggle(300);
     });


    //row hover on file browser
    $(".child_td").hover(
      function () {
        $(this).parent().children().each(function(index){
            $(this).removeClass("active_hover");
            $(this).addClass("active_hover");

        });
      },
      function () {
        $(this).parent().children().each(function(index){
            $(this).removeClass("active_hover");
        });
      }
    );

     //checkbox acl
     $(".rules").click(function(){
        var acl = $(this).text();
        if(acl[0] =='r') $(".group_read").attr('checked', true);
        if(acl[1] =='w') $(".group_write").attr('checked', true);
        if(acl[2] =='r') $(".all_read").attr('checked', true);
        if(acl[3] =='w') $(".all_write").attr('checked', true);
     });

     $(".group_read").click(function(){
        if(!$(this).attr('checked')){
            $(".group_write").removeAttr("checked");
            $(".all_read").removeAttr("checked");
            $(".all_write").removeAttr("checked");
        }
    });

    $(".group_write").click(function(){
        if(!$(this).attr('checked')){
            $(".all_write").removeAttr("checked");
        }
        else{
            $(".group_read").attr('checked', true);
        }
    });

    $(".all_read").click(function(){
        if(!$(this).attr('checked')){
            $(".all_write").removeAttr("checked");
        }
        else{
            $(".group_read").attr('checked', true);
        }
    });

    $(".all_write").click(function(){
        if($(this).attr('checked')){
            $(".group_read").attr('checked', true);
            $(".group_write").attr('checked', true);
            $(".all_read").attr('checked', true);
        }
    });

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
            $(".metadata_container").append('<tr class="metadata_form">'+
                                            '<td><input readonly class="metadata_key" value="'+ key + '"/></td>'+
                                            '<td><input type="text" class="metadata_value" value="'+ value + '"/></td>'+
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

     $( '.root ul' ).hide();
                             $( 'li.branch > a' ).prepend( '<i class="icon-chevron-up"></i>' );

                             $( 'li.branch > a' ).click( function ( event ) {
                                 event.preventDefault();

                                 var leaves = $( this ).parent( 'li' ).children( 'ul' );

                                 if ( leaves.is( ':visible' ) ) {
                                     $( 'i.icon-chevron-down', this ).removeClass( 'icon-chevron-down' ).addClass( 'icon-chevron-up' );
                                     leaves.slideUp();
                                 }
                                 else {
                                     $( 'i.icon-chevron-up', this ).removeClass( 'icon-chevron-up' ).addClass( 'icon-chevron-down' );
                                     leaves.slideDown();
                                 }
                             } );

                             $( '.collapse-all' ).click( function ( event ) {
                                 event.preventDefault();
                                 $( '.root ul' ).slideUp();
                                 $( 'li.branch > a > i.icon-chevron-down' ).removeClass( 'icon-chevron-down' ).addClass( 'icon-chevron-up' );

                             } );

                             $( '.expand-all' ).click( function ( event ) {
                                 event.preventDefault();
                                 $( '.root ul' ).slideDown();
                                 $( 'li.branch > a > i.icon-chevron-up' ).removeClass( 'icon-chevron-up' ).addClass( 'icon-chevron-down' );

                             } );





});