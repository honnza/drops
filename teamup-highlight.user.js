// ==UserScript==
// @name       TeamUp highlight
// @version    0.0.4
// @description  highlight the current event in a teamup schedule
// @match      *://teamup.com/*
// @copyright  2017+, Jan Dvorak
// @licence    Creative Commons with attribution
// ==/UserScript==

new MutationObserver(
  mrs => {if(mrs.some(mr => Array.from(mr.addedNodes).some(node => node.nodeName === "TABLE"))) refresh();}
).observe(document.body, {childList: true, subtree: true});
setInterval(refresh, 60000);
refresh();

function refresh(){
  if(!document.querySelector("#teamup-calendar-panel-tb-list.x-btn-pressed")) return;
  var dayRange = document.getElementById("teamup-calendar-panel-title").textContent;
  var dayEnd = new Date(dayRange.split(" - ")[1]);
  var now = Date.now();
  
  var day;
  document.querySelectorAll("#teamup-calendar-panel-list-bd tr").forEach(row => {
    if(!row.querySelector(".ext-cal-evt")) return;

    var dayEl = row.querySelector(".ext-cal-evt-agenda-time-ct .ext-cal-day-link");
    if(dayEl) {
      dayMatch = dayEl.id.match(/(\d\d\d\d)(\d\d)(\d\d)/);
      day = new Date(dayMatch[1], dayMatch[2] - 1, dayMatch[3]);
    }
    
    // The hour range always starts on the indicated day.
    // The day it ends on is sometimes present, sometimes not. 
    // When it is, it's always without the year. I think.
    var hoursRange = row.querySelector(".ext-cal-evt-hours").textContent;
    var hoursMatch = hoursRange.match(/(\d?\d):(\d\d) - (.*? )?(\d?\d):(\d\d)/);
    var timeStart = new Date(day);
    timeStart.setHours(hoursMatch[1]);
    timeStart.setMinutes(hoursMatch[2]);
    var timeEnd = hoursMatch[3] ? guessYear(hoursMatch[3], timeStart, 1) : new Date(day);
    timeEnd.setHours(hoursMatch[4]);
    timeEnd.setMinutes(hoursMatch[5]);
    
    // Now for for the interesting part:
    var progress = (now - timeStart) / (timeEnd - timeStart);
    
    var oldProgressEl = row.lastChild.querySelector("progress");
    if(oldProgressEl) oldProgressEl.remove();
    if(progress > 1) row.style.opacity = 0.5;
    else if(progress > 0){
      var progressEl = document.createElement("progress");
      progressEl.style.marginLeft = "1em";
      progressEl.value = progress;
      row.lastChild.appendChild(progressEl);
    }
  });
}

function guessYear(dayStr, guess, dir){
  if(guess instanceof Date) guess = guess.getYear() + 1900;
  for(offset = 0; offset < 100; offset++){
    var day = new Date(dayStr + " " + (guess + offset * dir));
    if(!isNaN(day.getDay())) return day;
  }
  debugger;
}