// ==UserScript==
// @name          Stack Exchange hover popup
// @description   Shows a popup with a SE question when a link to the question is hovered in the chat
// @require       http://cherne.net/brian/resources/jquery.hoverIntent.minified.js
// @include       http://chat.stackexchange.com/rooms/*
// @include       http://chat.meta.stackexchange.com/rooms/*
// @include       http://chat.stackoverflow.com/rooms/*
// @include       http://chat.meta.stackoverflow.com/rooms/*
// @version       0.3
// ==/UserScript==
$(function(){
  var popup = null;
  $("body").click(function(){
    if(popup) popup.parentNode.removeChild(popup);
    popup = null;
    console.log("frame removed");
  });
  $("#chat").hoverIntent({
    selector: ".content a", 
    out: function(){console.log("out");},
    over: function(event){
      function maximise(el){
        el.style.top = "0px";
        el.style.left = "0px";
        el.style.right = "0px";
        el.style.bottom = "0px";
        el.style.position = "absolute";
      }
      console.log("in %o", this);
      if(isSeDomain(this.host)){
        var frame = document.createElement("iframe");
        frame.style.height = "100%";
        frame.style.width = "100%";
        frame.style.margin = "auto";
        
        var border = document.createElement("div");
        border.style.margin = "10px";
        border.style.border = "3px solid black";
        border.style.borderRadius = "5px";
        border.style.padding = "10px";
        maximise(border);
        border.appendChild(frame);
        
        popup = document.createElement("div");
        popup.style.background = "rgba(0,0,0,0.5)";
        maximise(popup);
        popup.style.position = "fixed";
        popup.style.zIndex = 9;
        popup.appendChild(border);
        
        GM_xmlhttpRequest({
          url: this.href,
          onload: function(response){
            document.body.appendChild(popup);
            frame.contentDocument.innerHTML = response.responseText;
            console.log("%o appended", popup);
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