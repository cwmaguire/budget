"use strict";

function elem(id){
  return document.getElementById(id);
}

function draw_cumulative_graph(
  {x, y, w, h},
  {totalBudget,
   perPeriodBudget,
   currentPeriod,
   savedPrePeriod,
   savedThisPeriod,
   spentPrePeriod,
   spentThisPeriod}){
  let canvas = elem("canvas1");
  let ctx = canvas.getContext("2d");

  let innerH = Math.round(h * 0.4);
  let innerYTop = Math.round(y + Math.round(h * 0.1));
  let innerYBottom = Math.round(y + Math.round(h * 0.5));
  let saved = savedPrePeriod + savedThisPeriod;
  let spent = spentPrePeriod + spentThisPeriod;

  let offsetForOutline = 1;
  let total = 0;
  if(spent > saved){
    total = totalBudget + spent - saved;
  }else{
    total = totalBudget;
  }

  let widthFraction = (w - 2) / total;
  let zeroPos = x + offsetForOutline;
  if(spent > saved){
    zeroPos += Math.round(widthFraction * (spent - saved));
  }

  let savedPrePeriodW = Math.round(savedPrePeriod * widthFraction);
  let savedPrePeriodX = zeroPos;

  let savedThisPeriodW = Math.round(savedThisPeriod * widthFraction);
  let savedThisPeriodX = zeroPos + savedPrePeriodW;

  let spentPrePeriodW = Math.round(spentPrePeriod * widthFraction);
  let spentPrePeriodX = savedThisPeriodX + savedThisPeriodW - spentPrePeriodW;

  let spentThisPeriodW = Math.round(spentThisPeriod * widthFraction);

  let spentThisPeriodX = spentPrePeriodX - spentThisPeriodW;

  ctx.fillStyle = 'green';
  ctx.fillRect(savedThisPeriodX, innerYTop, savedThisPeriodW, innerH);

  ctx.fillStyle = 'darkgreen';
  ctx.fillRect(savedPrePeriodX, innerYTop, savedPrePeriodW, innerH);

  ctx.fillStyle = 'orange';
  ctx.fillRect(spentThisPeriodX, innerYBottom, spentThisPeriodW, innerH);

  ctx.fillStyle = 'brown';
  ctx.fillRect(spentPrePeriodX, innerYBottom, spentPrePeriodW, innerH);

  ctx.fillStyle = 'black';
  ctx.strokeRect(x, y, w, h);

  ctx.font = '12px Times New Roman';
  ctx.fillStyle = 'black';
  let savedPrePeriodTM = ctx.measureText('' + savedPrePeriod);
  let savedThisPeriodTM = ctx.measureText('' + savedThisPeriod);
  let spentPrePeriodTM = ctx.measureText('' + spentPrePeriod);
  let spentThisPeriodTM = ctx.measureText('' + spentThisPeriod);
  let totalBudgetTM = ctx.measureText('' + totalBudget);

  ctx.fillText('Dec 18, 2018', x, y - 2);
  ctx.fillText('' + totalBudget, x + w - totalBudgetTM.width, y + h + 10);

  ctx.fillStyle = 'white';
  if(savedPrePeriod > 0){
    ctx.fillText('' + savedPrePeriod, savedPrePeriodX + 3, innerYTop + 11);
  }
  if(savedThisPeriod > 0){
    ctx.fillText('' + savedThisPeriod, savedThisPeriodX + 3, innerYTop + 11);
  }
  if(spentPrePeriod){
    ctx.fillText('' + spentPrePeriod, spentPrePeriodX + 3, innerYBottom + 11);
  }
  ctx.fillStyle = 'black';
  if(spentThisPeriod > 0){
    ctx.fillText('' + spentThisPeriod, spentThisPeriodX + 3, innerYBottom + 11);
  }
  ctx.fillText('0', x, y + h + 10);
  if(spent > saved){
    ctx.fillText('' + (saved - spent), x, y + h + 10);
  }
}

function draw_bar_gauge({x, y, w, h}, amount, max){
  let canvas = elem("canvas1");
  let ctx = canvas.getContext("2d");
  let innerY = Math.round(y + Math.round(h * 0.1));
  let innerH = Math.round(h * 0.8);
  let innerW = Math.round(w / max * amount);

  // TODO: different colours for different states: under budget, close to budget, over budget
  ctx.fillStyle = 'orange';
  ctx.fillRect(x + 1, innerY, innerW, innerH);

  ctx.fillStyle = 'black';
  ctx.strokeRect(x, y, w, h);

  ctx.font = '12px Times New Roman';
  ctx.fillStyle = 'Black';
  let amountTextMetrics = ctx.measureText('' + amount);
  let maxTextMetrics = ctx.measureText('' + max);
  ctx.fillText('' + 'Dec 18, 2018', x, y - 2);
  ctx.fillText('' + max, x + w - maxTextMetrics.width, y + h + 10);

  let amountText = '' + amount;
  let amountX = x + innerW - amountTextMetrics.width - 3;
  let amountY = innerY + innerH - 3;
  ctx.fillText(amountText, amountX, amountY);
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
