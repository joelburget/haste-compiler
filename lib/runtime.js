var rts = require("6to5!./rts.js");
var floatdecode = require("6to5!./floatdecode.js");
var stdlib = require("6to5!./stdlib.js");
var endian = require("6to5!./endian.js");
var MVar = require("6to5!./MVar.js");
var StableName = require("6to5!./StableName.js");
var Integer = require("6to5!./Integer.js");
var Int64 = require("6to5!./Int64.js");
var md5 = require("./md5.js");
var array = require("./array.js");
var pointers = require("6to5!./pointers.js");
var cheapUnicode = require("./cheap-unicode.js");
var Canvas = require("./Canvas.js");
var Handle = require("./Handle.js");
var Weak = require("6to5!./Weak.js");

var files = [
    rts,
    floatdecode,
    stdlib,
    endian,
    MVar,
    StableName,
    Integer,
    Int64,
    md5,
    array,
    pointers,
    cheapUnicode,
    Canvas,
    Handle,
    Weak,
    ];

for (var i = 0; i < files.length; i++) {
    // HACK
    Object.assign(window, files[i]);
}
