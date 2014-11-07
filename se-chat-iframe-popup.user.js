// ==UserScript==
// @name          Stack Exchange hover popup
// @description   Shows a popup with a SE question when a link to the question is hovered in the chat
// @require       http://cherne.net/brian/resources/jquery.hoverIntent.minified.js
// @include       http://chat.stackexchange.com/rooms/*
// @include       http://chat.meta.stackexchange.com/rooms/*
// @include       http://chat.stackoverflow.com/rooms/*
// @include       http://chat.meta.stackoverflow.com/rooms/*
// @version       0.2
// ==/UserScript==
$(function(){
  var currentFrame = null;
  $("body").click(function(){
    if(currentFrame) currentFrame.parentNode.removeChild(currentFrame);
    currentFrame = null;
    console.log("frame removed");
  });
  $("#chat").hoverIntent({
    selector: ".content a", 
    out: function(){console.log("out");},
    over: function(event){
      console.log("in %o", this);
      if(isSeDomain(this.host)){
        console.log("domain matches");
        var overlay = document.createElement("div");
        overlay.style.background = "rgba(0, 0, 0, 0.5)";
        overlay.style.position = "fixed";
        overlay.style.top = 0;
        overlay.style.left = 0;
        overlay.style.right = 0;
        overlay.style.bottom = 0;
        
        var frame = document.createElement("iframe");

        currentFrame = document.createElement("div");
        currentFrame.style.margin = 5;
        currentFrame.style.border = "3px solid black";
        currentFrame.style.borderRadius = 5;
        currentFrame.style.padding = 10;
        currentFrame.appendChild(overlay);
        currentFrame.appendChild(frame);
        
        GM_xmlhttpRequest({
          url: this.href,
          onload: function(response){
            document.body.appendChild(currentFrame);
            frame.contentDocument.innerHTML = response.data;
            console.log("%o appended", currentFrame);
          }
        });
      }
    }
  });
  function isSeDomain(host){
    var hosts = ["stackexchange.com", 
                 "stackoverflow.com", 
                 "mathoverflow.com", 
                 "serverfault.com", 
                 "superuser.com", 
                 "stackapps.com", 
                 "askubuntu.com"]
    host = host.match(/\w+\.\w+$/)[0];
    return !!~hosts.indexOf(host);
  }
}());

/*
  var s = document.createElement("script");
  s.src = "http://cherne.net/brian/resources/jquery.hoverIntent.minified.js";
  document.head.appendChild(s);
*/