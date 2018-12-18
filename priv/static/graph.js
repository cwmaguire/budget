"use strict";

function elem(id){
  return document.getElementById(id);
}

function draw_cumulative_graph(min, max, dataPoints){
  let canvas = elem("canvas1");
  let ctx = canvas.getContext("2d");
  let h = canvas.height;
  let w = canvas.width;
  clear(ctx, w, h);
  ctx.fillStyle = rgb(state.frame);
  ctx.strokeStyle = "#F00";
  let i = 0;
  for(i = 100; i <= 100; i += 10){
    rotatedSquares(ctx,                       // The context of the canvas to paint with
                   state.frame - i,           // What frame we want to base the square off of
                   Math.floor(i / 10),        // Rate of increment for square color and angle
                   30,                        // How close to the right edge of the canvas the square can get
                   30,                        // How close to the bottom edge of the canvase the square can get
                   Math.floor((i - 100) / 5), // How many pixels per frame to move
                   w,                         // width of the canvas
                   h,                         // height of the canvas
                   i);                        // size of the square
  }
  return clone(state);
}

function draw_bar_gauge({x, y, w, h}, amount, max){
  let canvas = elem("canvas1");
  let ctx = canvas.getContext("2d");
  let innerY = Math.round(y + Math.round(h * 0.05));
  let innerH = Math.round(h * 0.9);
  let innerW = Math.round(w / max * amount);
  // TODO: if we've overspent then we need to draw this differently
  ctx.fillStyle = 'black';
  ctx.strokeRect(x, y, w, h);
  ctx.fillStyle = 'orange';
  ctx.fillRect(x + 1, innerY, innerW, innerH);
}


function draw_box(x, y){
  ctx.beginPath();
  ctx.moveTo(left + w / 2 + x, top + h / 2 - y);
  ctx.lineTo(left + w / 2 + y, top + h / 2 + x);
  ctx.lineTo(left + w / 2 - x, top + h / 2 + y);
  ctx.lineTo(left + w / 2 - y, top + h / 2 - x);
  ctx.closePath();
  ctx.stroke();
}

function draw_bar(x, y){
  ctx.beginPath();
  ctx.moveTo(left + w / 2 + x, top + h / 2 - y);
  ctx.lineTo(left + w / 2 + y, top + h / 2 + x);
  ctx.lineTo(left + w / 2 - x, top + h / 2 + y);
  ctx.lineTo(left + w / 2 - y, top + h / 2 - x);
  ctx.closePath();
  ctx.stroke();
}

function draw_max(max){

}

function draw_amt(amt){

}

function clear_canvas(){
  var c = elem("canvas1");
  var ctx = c.getContext("2d");
  var h = c.height;
  var w = c.width;
  ctx.clearRect(0, 0, h, w);
}
