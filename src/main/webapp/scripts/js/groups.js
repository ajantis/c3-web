function toggleTileView() {
    $("a").removeClass("active");
    $("#view-tiles").addClass("active");
    $('.container_groups').width('30%');
}

function toggleListView() {
    $("a").removeClass("active");
    $("#view-list").addClass("active");
    $('.container_groups').width('100%');
}

$(document).ready(function ($) {
    $.fn.editable.defaults.mode = "inline";
//                $('#postit').markItUp(mySettings);

    $('#comment_tags_input').editable({
        inputclass: 'input-large',
        select2: {
            tags: [],
            tokenSeparators: [","]
        },
        url: function (params) {
            updateCommentTagsCallback(params.value);
        }
    });

    $(':checkbox[name=check_all]').click(function () {
        $(':checkbox[name=check_file]').prop('checked', this.checked);
    });

    $('#description').editable({
        url: function(params) {
            updateDescriptionCallback(params.value);
        },
        rows: 2
    });

    $('#description_info').editable({
        url: function(params) {
            updateDescriptionCallback(params.value);
        },
        rows: 2
    });

    $('#firstname').editable({
        validate: function(value) {
           if($.trim(value) == '') return 'This field is required';
        }
    });

    $('#node_name').editable({
        url: function(params) {
            renameNodeCallback(params.value);
        },
        placement : 'bottom'
    });
    $('#edit_tags_form').editable({
        inputclass: 'input-large',
        select2: {
            tags: [],
            tokenSeparators: [","]
        },
        url: function(params) {
            updateTagsCallback(params.value);
        }
    });
    $( ".share_btn" ).click(function() {
        $("#sharing").removeClass("disp_none");
    });

    $( ".remove_public_link" ).click(function() {
        $("#sharing").addClass("disp_none");
    });

    $('#txtHash').focus(function(){
        $(this).attr('data-default', $(this).width());
        $(this).animate({ width: $(this).width() + 150 }, 'slow');
        $(this).select();
    }).blur(function(){
        var w = $(this).attr('data-default');
        $(this).animate({ width: w }, 'slow');
    });
    $( ".file-table tbody tr" ).on( "click", function( event ) {
        $("tr").removeClass("info");
        $( this ).addClass("info");
    });
    $('[data-toggle="tooltip"]').tooltip();
});