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
    return msg;
}

//get url param by name
function getParam(sParamName)
{
    var Params = location.search.substring(1).split("&");
    var variable = "";
    for (var i = 0; i < Params.length; i++){
        if (Params[i].split("=")[0] == sParamName)
        {
            if (Params[i].split("=").length > 1) variable = Params[i].split("=")[1];
            return variable;
        }
    }
    return "";
}

$(document).ready( function(){

    //update url
     $(".search_form").submit(function(){
        var url = "/index?query=" + $(".search_query").val();
        history.pushState(null, null, url);
     });

    //history (last view page)
     window.addEventListener("popstate", function(e) {
        $(".search_query").val(unescape(getParam("query")));
        $(".search_btn").submit();
     }, false)


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
//    $(".btn_add_metadata").live("click",function(){
//        var keyExist = true;
//        var key = $("#key").val();
//        var value = $("#value").val();
//        key = $.trim(key);
//        value = $.trim(value);
//        $(".metadata_key").each(function(index,el){
//            if(el.value==key) keyExist = false;
//        });
//        if(key!="" && value!="" && keyExist){
//            $("#key").val("");
//            $("#value").val("");
//            $(".metadata_container").append('<tr class="metadata_form">'+
//                                            '<td><input readonly class="metadata_key" value="'+ key + '"/></td>'+
//                                            '<td><input type="text" class="metadata_value" value="'+ value + '"/></td>'+
//                                            '<td><a class="close remove_metadata">&times;</a></td>'+
//                                       '</tr>');
//
//        }
//        if(!keyExist){
//            alert("This key is exists");
//        }
//
//    });
//
//    //select metadata from table
//     $(".btn_save_metadata").live("click",function(){
//        var key = ""
//        var value = ""
//        $(".metadata_key").each(function(index,el){
//            key += el.value +"%"
//
//        });
//        $(".metadata_value").each(function(index,el){
//                    value += el.value +"%"
//        });
//        $("#key_cont").val(key);
//        $("#value_cont").val(value);
//     });

    $('#left-panel').on('click','.label',function(){
                                                 var valueTag = $(this).text();
                                                 var input = $("#s_query");
                                                 if(input.val() == "")  input.val(valueTag);
                                                 else input.val(input.val() +" "+ valueTag);
                                              });
    $('#content').on('click','.tags_group',function(){
                                                     var valueTag = $(this).text();
                                                     var input = $("#s_query");
                                                     if(input.val() == "")  input.val(valueTag);
                                                     else input.val(input.val() +" "+ valueTag);
                                                  });
     //drop down left category
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
     });

     $( '.collapse-all' ).click( function ( event ) {
         event.preventDefault();
         $( '.root ul' ).slideUp();
         $( 'li.branch > a > i.icon-chevron-down' ).removeClass( 'icon-chevron-down' ).addClass( 'icon-chevron-up' );
     });

     $( '.expand-all' ).click( function ( event ) {
         event.preventDefault();
         $( '.root ul' ).slideDown();
         $( 'li.branch > a > i.icon-chevron-up' ).removeClass( 'icon-chevron-up' ).addClass( 'icon-chevron-down' );
     });
});