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
    $('#file_upload').fileupload( {
        form: "file_upload",
        autoUpload: false
    })

    $('#upload_form').modal({
        backdrop: true,
        keyboard: true,
        show: false
    }).css({
        // make width n * 10 % of screen
        'width': function () {
        return ($(document).width() * .7) + 'px';
        },
        // center model
        'margin-left': function () {
        return -($(this).width() / 2);
        }
    })
});

