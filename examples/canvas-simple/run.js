var createhidpi = require("./createhidpi.js");

var canvas = null;
var ctx = null;

function anim(stamp) {
    B(A(require("haskell!./canvas-simple.hs").animate,
        [[0, ctx, canvas], [0, stamp], 0]));
    window.requestAnimationFrame(anim);
}

window.onload = function () {
    canvas = document.getElementById("canvas");
    createhidpi(canvas, 400, 400);
    ctx = canvas.getContext('2d');

    window.requestAnimationFrame(anim);
};
