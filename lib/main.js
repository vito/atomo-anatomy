$(function(){
    if (!$(".slides").length)
        return;

    var initialPos = -512;

    if (window.location.hash) {
        initialPos -= 1124 * parseInt(window.location.hash.slice(1))
    }

    $(".slide").each(function(){
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

        if (e.keyCode == 39) {
            $(".slide").each(moveSlide(-1124));
        } else if (e.keyCode == 37) {
            $(".slide").each(moveSlide(1124));
        }
    });

    var startX, endX;
    $(".slides").bind("touchstart", function(e){
        startX = e.originalEvent.touches[0].pageX;
    });

    $(".slides").bind("touchmove", function(e){
        endX = e.originalEvent.touches[0].pageX;
        e.preventDefault();
    });

    $(".slides").bind("touchend", function(e){
        if ((startX - endX) > 50)
            $(".slide").each(moveSlide(-1124));
        else if ((endX - startX) > 50)
            $(".slide").each(moveSlide(1124));
    });

    function moveSlide(diff) {
        animating = true;
        return function(){
            var left = parseInt($(this).css("margin-left"), 10) + diff;
            $(this).animate({
                "margin-left": left,
                "opacity": opacity(left)
            }, {
                "duration": 250,
                "complete": (function(){
                    animating = false;
                    var pos = parseInt($(".slide:first").css("margin-left"), 10);
                    console.log(pos);
                    window.location.hash = "#" + Math.abs((pos + 512) / 1124);
                })
            });
        };
    }

    function opacity(n) {
        return (n == -512) ? 1 : 1;
    }
});
