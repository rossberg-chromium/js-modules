"use strict";
{
// Create:
const _ = {};
const __A = {};
const __A_B = {};
// Link:
Object.defineProperty(__A_B, "y", {get: function() { return __A_B_y }});
Object.freeze(__A_B);
Object.freeze(__A);
Object.freeze(_);
// Run:
let __A_x = __A_B_y;
let __A_B_y = __A_x;
print(__A_x);
}
