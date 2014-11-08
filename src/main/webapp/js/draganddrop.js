 $(document).ready(function(){
            if ($('.parent_link').attr('href').indexOf('files')==-1)
            {
                $('.parentfolder').hide()
            }
            if ($('.file-table-rules').children().length>1)
            {
                $('.rules').each(function()
                {
                    var dragCommand = $(this).attr('ondrop');
                    var row = $(this).parent().parent().parent();
                    row.addClass('drag');
                    row.addClass('drop');
                    row.attr('ondrop', $(this).attr('ondrop'))
                    row.attr('ondrag', $(this).attr('ondrag'))
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
            }
        });