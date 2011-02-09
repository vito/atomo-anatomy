$(function(){
    if (!$(".slides").length)
        return;

    var initialPos = -512;
    $(".slides .section").each(function(){
        $(this).css({
            "margin-left": initialPos,
            "opacity": opacity(initialPos)
        });
        initialPos += 1024 + 100;
    });

    var animating = false;
    $(".slides").keydown(function(e){
        console.log(e.keyCode, animating);
        if (animating)
            return;

        var pos = parseInt($("#content").css("left"), 10);
        if (e.keyCode == 39) {
            $(".slides .section").each(moveSlide(-1124));
        } else if (e.keyCode == 37) {
            $(".slides .section").each(moveSlide(1124));
        }
    });

    function moveSlide(diff) {
        animating = true;
        return function(){
            var left = parseInt($(this).css("margin-left"), 10);
            $(this).animate({
                "margin-left": left + diff,
                "opacity": opacity(left + diff)
            }, {
                "duration": 250,
                "complete": (function(){
                    animating = false;
                })
            });
        };
    }

    function opacity(n) {
        return (n == -512) ? 1 : 1;
    }
});