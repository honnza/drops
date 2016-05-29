// ==UserScript==
// @name       SOChatUserColors
// @version    1.6.2
// @description  color chat lines in a Stack Overflow chat room, using a different color for each user
// @match      *://chat.stackoverflow.com/rooms/*
// @match      *://chat.stackexchange.com/rooms/*
// @match      *://chat.meta.stackoverflow.com/rooms/*
// @match      *://chat.meta.stackexchange.com/rooms/*
// @match      *://chat.stackoverflow.com/transcript/*
// @match      *://chat.stackexchange.com/transcript/*
// @match      *://chat.meta.stackoverflow.com/transcript/*
// @match      *://chat.meta.stackexchange.com/transcript/*
// @copyright  2012+, Jan Dvorak
// @licence    Creative Commons with attribution
// ==/UserScript==

var main = function(){
  var FORCE_SCALE = 0.5;
  var $presentUsers = $("#present-users");
  var $chat = $(".monologue").parent().add("#chat");
  var $style = $("<style>");
  var isMobile = /( |^)mc=1/.test(document.cookie);

  setInterval(refresh, 1000);
  $style.appendTo(document.head);

  function refresh(){
    var newCSS = "";
    var selectorRest = isMobile ? "" : " .messages";
    function colorise(usrId){
      var usrIDbits = (+/\d+/.exec(usrId)).toString(2).split('').reverse().join('');
      return [parseInt(('11'+usrIDbits.replace(/(.)?.?.?/g,'$1')+"00000000").slice(0,8),2),
      parseInt(('11'+usrIDbits.replace(/.?(.)?.?/g,'$1')+"00000000").slice(0,8),2),
      parseInt(('11'+usrIDbits.replace(/.?.?(.)?/g,'$1')+"00000000").slice(0,8),2)];
    }

    var users = CHAT.RoomUsers.all();
    users.forEach(function(user){
      if(!user.color){ user.color = colorise(user.id); }
      var usrClass = "#main .monologue.user-"+user.id;
      user.msgCount = $(usrClass + selectorRest).length;
    });
    users.forEach(function(user){
      user.cDiff = [0, 0, 0];
      users.forEach(function(user2) {
        if(user !== user2) {
          var dx = (user.color[0]-user2.color[0]);
          var dy = (user.color[1]-user2.color[1]);
          var dz = (user.color[2]-user2.color[2]);
          var force = FORCE_SCALE / (dx*dx + dy*dy + dz*dz) * user.msgCount * user2.msgCount;
          user.cDiff[0] += dx * force;
          user.cDiff[1] += dy * force;
          user.cDiff[2] += dz * force;
        }
      });
    });
    users.forEach(function(user){
      user.color[0] = 224 + 32 * Math.tanh((user.color[0] + user.cDiff[0]) / 32 - 7);
      user.color[1] = 224 + 32 * Math.tanh((user.color[1] + user.cDiff[1]) / 32 - 7);
      user.color[2] = 224 + 32 * Math.tanh((user.color[2] + user.cDiff[2]) / 32 - 7);
      var colorCSS = "rgb(" + user.color.map(v => Math.floor(v)).join(", ") + ")";
      var usrClass = "#main .monologue.user-"+user.id;
      newCSS += usrClass + selectorRest + "{background-color:"+colorCSS+"}\n";
    });

    $style.text(newCSS);
  }
};

//the following lines were donated by Phenomnomnominal
var script = document.createElement('script');
script.type = "text/javascript";
script.textContent = '(' + main.toString() + ')();';
document.body.appendChild(script);
