 $(document).ready(function() {

    $(".template-upload").on("click",".start_upload",function(){
        var parent = $(this).parent().parent().parent();
        var progress_bar = parent.children(".progress_error").children(".progress");
        progress_bar.show();
    })
     $(".template-upload-replace").on("click",".start_upload",function(){
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


    $('#file_replace_form').fileupload(
    {
        done:function (e, data) {
                     var fileName = ""
                     $.each(data.files, function (i, file) {
                         fileName = file.name;
                         console.log(file.name + " was uploaded")
                     });
                     $('#upload_form').modal("hide");
                     var url = window.location.href;
                      window.location = url.replace(url.substr(url.lastIndexOf('/') + 1), '')+fileName;
         },
         uploadTemplateId: 'template-upload-replace',
         downloadTemplateId: 'template-download-replace'
    });

    $('#replace_form').modal({
        backdrop: true,
        keyboard: true,
        show: false
    })

     $('.add-replace-file').change(function() {

           if ((this).files.length == 1)
                $(".fileinput-button").attr('disabled','disabled');
           else
                $(".fileinput-button").removeAttr('disabled')
     });
});