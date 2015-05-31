 $(document).ready(function(){
    if ($('.parent_link').attr('href') && $('.parent_link').attr('href').indexOf('files')==-1)
    {
        $('.parentfolder').hide()
    }
    else
    {
        var parentDropAttr = $('.parentfolder').attr('ondrop')
        if (parentDropAttr == "" || parentDropAttr == undefined)
        {
             $('.parentfolder').removeClass('drop')
        }
    }

    $('.acl_cont').each(function()
    {
        var row = $(this).parent();

        var dragAttr = $(this).attr('ondrag')
        var dropAttr = $(this).attr('ondrop')

        if (dragAttr != "" && dragAttr != undefined)
        {
            row.addClass('drag');
            row.attr('ondrag', dragAttr)
        }
        if (dropAttr != "" && dropAttr != undefined)
        {
            row.addClass('drop');
            row.attr('ondrop', dropAttr)
        }
    });

    $('.drag').draggable({
            helper : 'clone',
            opacity : 0.6,
    });

    $('.drop').droppable({
            drop : function(event, ui) {
                ui.draggable.remove();
            }
    });
})