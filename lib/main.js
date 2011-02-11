var animating = false,
    position = 0;

var X_OFFSET = 50,
    Y_OFFSET = 50,
    SLIDE_WIDTH,
    SLIDE_HEIGHT;

$(function(){
    if (!$(".slides").length)
        return;

    SLIDE_WIDTH = $(".slide").outerWidth();
    SLIDE_HEIGHT = $(".slide").outerHeight();

    positionSlides();
    bindKeys();
    bindTouch();
});

function positionSlides() {
    var initialPos = -(SLIDE_WIDTH / 2);

    if (window.location.hash) {
        position = parseInt(window.location.hash.slice(1));
        initialPos -= (SLIDE_WIDTH + X_OFFSET) * position;
    }

    $(".slide:not(.continue)").each(function(i){
        $(this).css("margin-left", initialPos);
        $(this).addClass("group-" + i);

        var yPos = (SLIDE_HEIGHT / 2) + Y_OFFSET;
        $(this).nextUntil(":not(.continue)").each(function(){
            $(this).css({
                "margin-left": initialPos,
                "margin-top": yPos
            });

            $(this).addClass("group-" + i);

            yPos += (SLIDE_HEIGHT + Y_OFFSET);
        });

        initialPos += SLIDE_WIDTH + X_OFFSET;
    });
}

function bindKeys() {
    $(".slides").keydown(function(e){
        if (animating)
            return;

        if (e.keyCode == 39)
            $(".slide").each(moveX(-1));
        else if (e.keyCode == 37)
            $(".slide").each(moveX(1));
        else if (e.keyCode == 40)
            $(".group-" + position).each(moveY(-1));
        else if (e.keyCode == 38)
            $(".group-" + position).each(moveY(1));
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
            $(".slide").each(moveX(-1));
        else if ((endX - startX) > 50)
            $(".slide").each(moveX(1));
    });
}

function moveX(diff) {
    animating = true;
    position -= diff;
    return function(){
        $(this).animate({
            "margin-left": getLeft($(this)) + ((SLIDE_WIDTH + X_OFFSET) * diff)
        }, {
            "duration": 250,
            "complete": (function(){
                animating = false;
                window.location.hash = position;
            })
        });
    };
}

function moveY(diff) {
    animating = true;
    return function(){
        $(this).animate({
            "margin-top": getTop($(this)) + ((SLIDE_HEIGHT + Y_OFFSET) * diff)
        }, {
            "duration": 250,
            "complete": (function(){
                animating = false;
            })
        });
    };
}

function getLeft(ele) {
    return parseInt(ele.css("margin-left"), 10);
}

function getTop(ele) {
    return parseInt(ele.css("margin-top"), 10);
}