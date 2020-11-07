// ==UserScript==
// @name       SOChatUserColors
// @version    2.0
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
  var FORCE_SCALE = 50;
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
        var usrHash = {};
        $(".username a").each(function(){
          var urlParts = this.pathname.split("/").slice(-2);
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
        if(!user.colorFixed){
          user.color[0] += user.cDiff[0];
          user.color[1] += user.cDiff[1];
          user.color[2] += user.cDiff[2];
          var dx = user.color[0] - 224, dy = user.color[1] - 224, dz = user.color[2] -224;
          var dist = Math.sqrt(dx * dx + dy * dy + dz * dz), scale = 32/dist;
          user.color[0] = user.color[0] * scale + 224 * (1 - scale);
          user.color[1] = user.color[1] * scale + 224 * (1 - scale);
          user.color[2] = user.color[2] * scale + 224 * (1 - scale);
        debugLines.push(user.msgCount + " [" + showAry(user.color, 15) + "] " + dist.toFixed(15) + " " + scale.toFixed(15) + " " + user.name)
        } else {
                  debugLines.push(user.msgCount + " [" + showAry(user.color, 15) + "] ------------------ ------------------ " + user.name)

        }
        var colorCSS = "rgb(" + showAry(user.color, 0) + ")";
        var usrClass = ".monologue.user-"+user.id;
        newCSS += usrClass + selectorRest + "{background-color:" + colorCSS + "}\n";
      });

      $style.text(newCSS);
      $debug.text(debugLines.join("\n"));
      }
    setTimeout(refresh, 100);
  }

  // color picker functionality

  function uvw2rgb(u, v, w = 32){
    // (0, 1) should point in hat(2, -1, -1); (1, _) should point in hat(1, 1, 1)
    var cu = w * Math.SQRT1_2 * Math.cos(v);
    var l = w * Math.sqrt(1/3) * Math.sin(v) + 224;
    return [
      l + cu * Math.cos(u),
      l + cu * Math.cos(u - 2/3 * Math.PI),
      l + cu * Math.cos(u + 2/3 * Math.PI),
    ];
  }

  function rgb2uvw(r, g, b){
    var l = (r + g + b) / 3;
    var w = Math.hypot(r - 224, g - 224, b - 224);
    return [
      Math.atan2(g - b, Math.sqrt(3) * (r - l)),
      Math.asin(Math.sqrt(3) * (l - 224) / w),
      w
    ];
  }

  new MutationObserver(evts => {
    evts.forEach(evt => {
      evt.addedNodes.forEach(node => {
        if(node.classList.contains("user-popup")){
          new MutationObserver(evts => {
            evts.forEach(evt => {
              if(Array.from(evt.addedNodes).some(node => node.textContent && node.textContent.includes("Actions"))){
                fillUserPopup(node)
              }
            });
          }).observe(node, {childList: true});
        }
      });
    })
  }).observe(document.getElementById("chat-body"), {childList: true});

  function fillUserPopup(node){
    var userId = +$("a[href ^= '/users']", node).prop("href").match(/\d+/);
    CHAT.RoomUsers.get(userId).then(user => {
      $("<div><a href = '#'>set user color</a></div>").appendTo(node)
      .find("a").on("click", function(e){
        e.preventDefault();
        var popup = popUp(e.pageX, e.pageY).addClass("picker-popup").css({width: "auto"});

        var canvas = pickerCanvas(360, 180);
        $(canvas).on("mousedown mousemove", e => {
          if(e.buttons == 1){
            $crosshair.css({left: e.offsetX, top: e.offsetY});
          }
        }).appendTo(popup).wrap($("<div>").css({position: "relative", marginTop: 20}));

        // image taken from https://uxwing.com/target-focus-line-icon/
        var $crosshair = $("<img src = 'https://raw.githubusercontent.com/honnza/drops/master/target-focus-line.svg'>").css({
          position: "absolute", width: 17, height: 17, margin: -8, pointerEvents: "none"
        }).insertBefore(canvas);
        var [u, v] = rgb2uvw(...user.color);
        console.log(user.color, [u, v]);
        if(u < 0) u += 2 * Math.PI;
        $crosshair.css({
          left: u * canvas.width / (2 * Math.PI),
          top: (v / Math.PI + 0.5) * canvas.height
        });

        $("<div><a>set color</a></div>").appendTo(popup)
        .find("a").on("click", function(e){
          e.preventDefault();
          user.colorFixed = true;
          user.color = uvw2rgb(
            2 * Math.PI * $crosshair.position().left / (canvas.width - 1),
            Math.PI * $crosshair.position().top / (canvas.height - 1) - Math.PI/2
          );
          node.fadeOut(() => node.remove());
        });

        $("<div><a>enable dynamic coloration</a></div>").appendTo(popup)
        .find("a").on("click", function(e){
          e.preventDefault();
          user.colorFixed = false;
          node.fadeOut(() => node.remove());
        });
      });
    });
  }

  function pickerCanvas(width, height){
    var canvas = document.createElement("canvas");
    canvas.width = width;
    canvas.height = height;
    var ctx = canvas.getContext("2d");
    var imageData = ctx.createImageData(width, height);
    for(var y = 0; y < height; y++){
      var v = y * Math.PI / (height - 1) - Math.PI / 2;
      for(var x = 0; x < width; x++){
        var u = x * 2 * Math.PI / (width - 1);
        var i = 4 * (x + width * y);
        [imageData.data[i], imageData.data[i+1], imageData.data[i+2]] = uvw2rgb(u, v);
        [imageData.data[i+3] = 255];
      }
    }
    ctx.putImageData(imageData, 0, 0);
    return canvas;
  }
};

//the following lines were donated by Phenomnomnominal
var script = document.createElement('script');
script.type = "text/javascript";
script.textContent = '(' + main.toString() + ')(' + (typeof unsafeWindow === 'undefined') + ');';
document.body.appendChild(script);