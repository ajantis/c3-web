function HideGroupDetails(that)
{
    var panel = $(that).parent().parent().children().eq(2);
    var btn = $(that);
   // panel.toggle();

    if (panel.css('height')=='10px')
    {
        panel.css('height', 'auto');
        btn.attr("src", "/images/icon-chevron-down.png");
    }
    else
    {
        panel.css('height', '10px');
        btn.attr("src", "/images/icon-chevron-up.png");
    }
}
function HideGroupDetails2(that)
{
    var panel = $(that).parent().parent().children('.desc_panel');
    var btn = $(that);

    if ((panel).css('height')=='25px')
    {
        (panel).css('height','auto')
        btn.attr("class", "toggle_switch inf_right icon-chevron-up");
    } else {
        (panel).css('height','25px')
        btn.attr("class", "toggle_switch inf_right icon-chevron-down");
    }
}

function switchRowsViewOn() {
    $('.container_groups').removeClass('shadow-z-2').removeClass('group-tile');
}

function switchTileViewOn() {
    $('.container_groups').addClass('shadow-z-2').addClass('group-tile');
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

    $.fn.editable.defaults.mode = 'popover';
    $('#description').editable({
        url: function(params) {
            updateDescriptionCallback(params.value);
        },
        rows: 2
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
  });