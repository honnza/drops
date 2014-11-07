// ==UserScript==
// @name          Stack Exchange hover popup
// @description   Shows a popup with a SE question when a link to the question is hovered in the chat
// @require       http://cherne.net/brian/resources/jquery.hoverIntent.minified.js
// @include       http://*stackexchange.com/*
// @include       http://*stackoverflow.com/*
// @include       http://*mathoverflow.com/*
// @include       http://*serverfault.com/*
// @include       http://*superuser.com/*
// @include       http://*stackapps.com/*
// @include       http://*askubuntu.com/*
// @version       2.2
// ==/UserScript==
$(function(){
  var currentFrame = null;
  $("body").click(function(){
    if(currentFrame) currentFrame.parentNode.removeChild(currentFrame);
    currentFrame = null;
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
        console.log("%o appended", currentFrame);
        
         GM_xmlhttpRequest({
          url: link.href,
          onload: function(response){
            frame.document.documentElement.innerHTML = response.data;
           document.body.appendChild(currentFrame);
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