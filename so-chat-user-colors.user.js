// ==UserScript==
// @name       SOChatUserColors
// @version    1.6
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
  var FORCE = 1;
  var $presentUsers = $("#present-users");
  var $chat = $(".monologue").parent().add("#chat");
  var $style = $("<style>");
  var isMobile = /( |^)mc=1/.test(document.cookie);

  //element not present in the transcript
  $presentUsers.each(function(){
    new MutationObserver(refresh).observe(this, {childList:true, subtree:true});
  });
  var pastUsersMO = new MutationObserver(addPastUsers);
  pastUsersMO.observe($chat[0], {childList:true, subtree:true});
  $style.appendTo(document.head);
  var pastUsers = {};
  addPastUsers();

  function addPastUsers(records){
    //currently (nov 2013) a profile link is added even before user data is loaded.
    //In case this ever changes, the second condition will kick in
    if($(".monologue").length && !$(".username:not(a *):not(:has(a))").length){
      pastUsersMO.disconnect();
    }
    $(".monologue").each(function(){
      var $profileLink = $(".username a, a:has(.username)", this);
      if($profileLink.length){
        var id = $profileLink.attr("href").match(RegExp("/users/(-?\\d+)"))[1];
        if (!users[id] && !pastUsers[id]){
          pastUsers[id] = {id:id, name: $profileLink.attr("title")};
        }
      };
    });
    refresh();
  }

  function refresh(){
    var newCSS = "";
    var selectorRest = isMobile ? "" : " .messages";
    function colorise(usrId){
      var usrIDbits = (+/\d+/.exec(usrId)).toString(2).split('').reverse().join('');
      return = [parseInt(('11'+usrIDbits.replace(/(.)?.?.?/g,'$1')+"00000000").slice(0,8),2),
      parseInt(('11'+usrIDbits.replace(/.?(.)?.?/g,'$1')+"00000000").slice(0,8),2),
      parseInt(('11'+usrIDbits.replace(/.?.?(.)?/g,'$1')+"00000000").slice(0,8),2)];
    }

    var users = CHAT.RoomUsers.allPresent();
    users.forEach(function(user){
      if(!user.color && !pastUsers[user.id]){
        user.color = colorise(user.id);
      }
    });
    users.forEach(function(user){
      users.forEach(function(user2){
        user.cDiff = [(user.color[0]-user2.color[0]) * FORCE,
        (user.color[1]-user2.color[1]) * FORCE,
        (user.color[2]-user2.color[2]) * FORCE];
      })
    });
    users.forEach(function(user){
      user.color[0] = 224 + 32 * Math.atanh((user.color[0] + user.cDiff[0]) / 32 - 7);
      user.color[1] = 224 + 32 * Math.atanh((user.color[1] + user.cDiff[1]) / 32 - 7);
      user.color[2] = 224 + 32 * Math.atanh((user.color[2] + user.cDiff[2]) / 32 - 7);
      var colorCSS = "rgb(" + user.color.join(", ") + ")";
      var usrClass = "#main .monologue.user-"+usrId;
      newCSS += usrClass + selectorRest + "{background-color:"+usrColor+"}\n";
    });
    for(u in pastUsers){
      if(!pastUsers[u].colorised){
        colorise(u);
        pastUsers[u].colorised = true;
      }
    }

    if(newCSS) {$style.text($style.text()+newCSS)};
  }
};

//the following lines were donated by Phenomnomnominal
var script = document.createElement('script');
script.type = "text/javascript";
script.textContent = '(' + main.toString() + ')();';
document.body.appendChild(script);
