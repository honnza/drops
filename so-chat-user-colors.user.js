// ==UserScript==
// @name       SOChatUserColors
// @version    1.7.4
// @description  color chat lines in a Stack Overflow chat room, using a different color for each user
// @match      *://chat.stackoverflow.com/rooms/*
// @match      *://chat.stackoverflow.com/search*
// @match      *://chat.stackexchange.com/rooms/*
// @match      *://chat.stackexchange.com/search*
// @match      *://chat.meta.stackoverflow.com/rooms/*
// @match      *://chat.meta.stackoverflow.com/search*
// @match      *://chat.meta.stackexchange.com/rooms/*
// @match      *://chat.meta.stackexchange.com/search*
// @match      *://chat.stackoverflow.com/transcript/*
// @match      *://chat.stackexchange.com/transcript/*
// @match      *://chat.meta.stackoverflow.com/transcript/*
// @match      *://chat.meta.stackexchange.com/transcript/*
// @copyright  2012+, Jan Dvorak
// @licence    Creative Commons with attribution
// ==/UserScript==

var main = function(showDebug){
  var FORCE_SCALE = 100;
  var $presentUsers = $("#present-users");
  var $chat = $(".monologue").parent().add("#chat");
  var $style = $("<style>");
  var $debug = $("<pre><span>")
      .css({position: "fixed", display: "inline-block", right: 25, top: 25, zIndex: 1,
            background: "white", border: "2px solid darkGray", borderRadius: 10, padding: 10})
      .on("click", function(){$debug.toggle();});
  if(showDebug) $debug.appendTo(document.body);
  $debug = $debug.find("span");
  var isMobile = /( |^)mc=1/.test(document.cookie);

  refresh();
  $style.appendTo(document.head);

  function showAry(ary, n){return ary.map(v => v.toFixed(n)).join(", ")}

  function refresh(){
    var newCSS = "";
    var debugLines = [];
    var selectorRest = isMobile ? "" : " .messages";
    function colorise(usrId){
      var usrIDbits = (+/\d+/.exec(usrId)).toString(2).split('').reverse().join('');
      return [parseInt(('11'+usrIDbits.replace(/(.)?.?.?/g,'$1')+"00000000").slice(0,8),2),
      parseInt(('11'+usrIDbits.replace(/.?(.)?.?/g,'$1')+"00000000").slice(0,8),2),
      parseInt(('11'+usrIDbits.replace(/.?.?(.)?/g,'$1')+"00000000").slice(0,8),2)];
    }

    if(!document.visibilityState || document.visibilityState === "visible"){
      // So... the main chat page has this object populated with users and live.
      // The transcript also has this object, but with no-one inside.
      // The search result page also has a CHAT.RoomUsers objecct, but it's empty.
      var users = CHAT.RoomUsers.all && CHAT.RoomUsers.all();
      if(!users || (users.count && users.count() === 0)){
        usrHash = {};
        $(".username a").each(function(){
          urlParts = this.pathname.split("/").slice(-2);
          usrHash[urlParts[1]] = {id: +urlParts[0]}
        });
        users = Object.values(usrHash);
      }
      // Firefox scrolls the document, Chrome scrolls its body
      var sTop = document.documentElement.scrollTop + document.body.scrollTop;
      var sBot = sTop + window.innerHeight - $("#input-area").height();
      users.forEach(function(user){
        if(!user.color){ user.color = colorise(user.id); }
        var usrClass = ".monologue.user-"+user.id;
        user.msgCount = $(usrClass + selectorRest)
            .filter((i, e) => {
              var eTop = $(e).offset().top, eBot = eTop + e.offsetHeight;
              return eBot > sTop && eTop < sBot;
            }).length;
      });
      users = users.filter(u => u.msgCount);
      users.forEach(function(user){
        user.cDiff = [0, 0, 0];
        users.forEach(function(user2) {
          if(user !== user2) {
            var dx = (user.color[0]-user2.color[0]);
            var dy = (user.color[1]-user2.color[1]);
            var dz = (user.color[2]-user2.color[2]);
            var sqDist = (dx*dx + dy*dy + dz*dz)
            var forceMul = FORCE_SCALE / sqDist * (user.msgCount * user2.msgCount);
            user.cDiff[0] += dx * forceMul;
            user.cDiff[1] += dy * forceMul;
            user.cDiff[2] += dz * forceMul;
          }
        });
      });
      users.forEach(function(user){
        user.color[0] += user.cDiff[0];
        user.color[1] += user.cDiff[1];
        user.color[2] += user.cDiff[2];
        var dx = user.color[0] - 224, dy = user.color[1] - 224, dz = user.color[2] - 224;
        var dist = Math.sqrt(dx * dx + dy * dy + dz * dz), scale = 32/dist;
        user.color[0] = user.color[0] * scale + 224 * (1 - scale);
        user.color[1] = user.color[1] * scale + 224 * (1 - scale);
        user.color[2] = user.color[2] * scale + 224 * (1 - scale);
        var colorCSS = "rgb(" + showAry(user.color, 0) + ")";
        var usrClass = ".monologue.user-"+user.id;
        debugLines.push(user.msgCount + " [" + showAry(user.color, 15) + "] " + dist.toFixed(15) + " " + scale.toFixed(15) + " " + user.name)
        newCSS += usrClass + selectorRest + "{background-color:" + colorCSS + "}\n";
      });

      $style.text(newCSS);
      $debug.text(debugLines.join("\n"));
	  }
    setTimeout(refresh, 100);
  }
};

//the following lines were donated by Phenomnomnominal
var script = document.createElement('script');
script.type = "text/javascript";
script.textContent = '(' + main.toString() + ')(' + (typeof unsafeWindow === 'undefined') + ');';
document.body.appendChild(script);
