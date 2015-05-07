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
            });