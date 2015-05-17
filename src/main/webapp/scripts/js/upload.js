 $(document).ready(function() {
    $(".template-upload").on("click",".start_upload",function(){
        var parent = $(this).parent().parent().parent();
        var progress_bar = parent.children(".progress_error").children(".progress");
        progress_bar.show();
    })
    $(".template-upload").on("keyup",".description", function(){
        var parent = $(this).parent().parent().parent();
        var start_button = parent.children(".tag_start_stop").children(".start_stop").children(".start").children();
        var description_text = $(this).val();
        if(description_text.length > 0){
            start_button.show();
        }
        else {start_button.hide();}
    })

     $('#file_upload_form').fileupload({
         done:function (e, data) {
             $.each(data.files, function (i, file) {
                 console.log(file.name + " was uploaded")
             });
             $('#upload_form').modal("hide");
             location.reload();
         },
         send: function (e, data) {
                  var fields = $(".item-required")
                         .find("textarea, input").serializeArray();

                   $.each(fields, function(i, field) {
                     if (!field.value)
                       alert(field.name + ' is required');
                       event.preventDefault();
                    });
         }
     });
    $('#upload_form').modal({
        backdrop: true,
        keyboard: true,
        show: false
    })
});

