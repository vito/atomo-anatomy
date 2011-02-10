var animating = false;

$(function(){
    if (!$(".slides").length)
        return;

    positionSlides();
    bindKeys();
    bindTouch();
});

function positionSlides() {
    var initialPos = -512;

    if (window.location.hash)
        initialPos -=
            1124 * parseInt(window.location.hash.slice(1));

    $(".slide").each(function(){
        $(this).css({ "margin-left": initialPos });
        initialPos += 1124;
    });
}

function bindKeys() {
    $(".slides").keydown(function(e){
        if (animating)
            return;

        if (e.keyCode == 39)
            $(".slide").each(moveSlide(-1));
        else if (e.keyCode == 37)
            $(".slide").each(moveSlide(1));
    });
}

function bindTouch() {
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
            $(".slide").each(moveSlide(-1));
        else if ((endX - startX) > 50)
            $(".slide").each(moveSlide(1));
    });
}

function moveSlide(diff) {
    animating = true;
    return function(){
        $(this).animate({
            "margin-left": getLeft($(this)) + (1124 * diff)
        }, {
            "duration": 250,
            "complete": (function(){
                animating = false;
                window.location.hash =
                    "#" + -((getLeft($(".slide:first")) + 512) / 1124);
            })
        });
    };
}

function getLeft(ele) {
    return parseInt(ele.css("margin-left"));
}