// ==UserScript==
// @name          Unofficial Stack Exchange gas mask
// @description   A mission-critical tool when exploring the deepest depths of Stack Exchange.
// @description   Automatically hides images from new users.
// @run-at        document-start
// @grant         none
// @include       *://*stackexchange.com/questions/*
// @include       *://*stackoverflow.com/questions/*
// @include       *://*mathoverflow.com/questions/*
// @include       *://*serverfault.com/questions/*
// @include       *://*superuser.com/questions/*
// @include       *://*stackapps.com/questions/*
// @include       *://*askubuntu.com/questions/*
// @version       1.3.1
// ==/UserScript==

if(true || location.search.indexOf("smokeypost=true") !== -1){
  var style = document.createElement("style");
  style.textContent = ".post-text img:not(.gasmask-treated){visibility:hidden}" +
                      ".post-text img{cursor:pointer}";
  document.head.append(style);

  var timer = setInterval(function(){
    if(document.readyState === "complete") clearInterval(timer);
    var newImgs = document.querySelectorAll(".post-text img:not(.gasmask-treating)");
    [].forEach.call(newImgs, function(img){
      var post = img;
      while(!post.classList.contains("postcell") && !post.classList.contains("answercell")) post = post.parentElement;
      var repElem = post.querySelector(".post-signature:last-child .reputation-score");
      if(repElem.textContent === "1"){
        var origSrc = img.src;
        img.src = "https://upload.wikimedia.org/wikipedia/commons/thumb/5/57/Gas_mask.svg/200px-Gas_mask.svg.png";
        img.addEventListener("click", function handler(event){
          img.src = origSrc;
          img.removeEventListener("click", handler);
          event.preventDefault();
        });
        img.classList.add("gasmask-treating");
        setTimeout(function(){
          img.classList.add("gasmask-treated");
        }, 1000);
      } else {
        img.classList.add("gasmask-treating", "gasmask-treated");
      }
    });
  }, 100);
}

// GasMask key
// 975753bb33f9300e0a21ae1f95f63445308562bd5f2e15bf4239eefc8c701b54
// filter for URL
// %00%00%00%00%00%00%00%00%40%00%00%00%00%00%00%00
// API URL
// https://metasmoke.erwaysoftware.com/api/posts/url
