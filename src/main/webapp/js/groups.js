function HideGroupDetails(that)
{
    var panel = $(that).parent().parent().children().eq(2);
    var btn = $(that);
   // panel.toggle();

    if (panel.css('height')=='10px')
    {
        panel.css('height')='auto'
        btn.attr("src", "/images/icon-chevron-down.png");
    }
    else
    {
        panel.css('height')='10px'
        btn.attr("src", "/images/icon-chevron-up.png");
    }
}
