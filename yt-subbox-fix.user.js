// ==UserScript==
// @name          Youtube subbox number patch
// @description   replaces the (unreliable and since mid-Nov2015 broken) subscription feed new item counter in the menu
// @include       http*://*.youtube.com/*
// @version       1.0.0
// ==/UserScript==

if(location.pathname === "/feed/subscriptions"){
  GM_setValue("seenVids", JSON.stringify(scrapeIDs(document)));
} else {
   var seenVids = JSON.parse(GM_getValue("seenVids"));
  var xhr = new XMLHttpRequest();
  xhr.open("get", "/feed/subscriptions");
  xhr.onload = function(){
    var subVids = scrapeIDs(new DOMParser().parseFromString(xhr.response, "text/html"));
    var numUnseen = subVids.filter(id => !~seenVids.indexOf(id)).length;
    awaitElement(document.getElementById("guide"), ".guide-count-value", function(numElement){
       numElement.textContent = numUnseen.toString();
    })
  };
  xhr.onerror = console.error.bind(console);
  xhr.send();
}

function scrapeIDs(document){
  return [].map.call(document.querySelectorAll(".section-list .yt-lockup"), elem => elem.dataset.contextItemId)
}

function awaitElement(parent, selector, callback){
   var node = parent.querySelector(selector); 
   if(node){
      callback(node);
   }else{
      mo = new MutationObserver(function(records){
         records.forEach(function(record){
            [].some.call(record.addedNodes, function(node){
               if(node.nodeType === node.ELEMENT_NODE){
                  if(! node.matches(selector)){
                     node = node.querySelector(selector);
                  }
                  if(node){
                    callback(node);
                    mo.disconnect();
                    return true;
                  }
               }
               return false;
            });
         });
      });
      mo.observe(parent, {childList:true, subtree:true})
   }
}

// With a guide count, the DOM is:
//  body#body > div#body-container > div#page-container > div#page > div#guide > div#appbar-guide-menu 
//   > div#guide-container > div.guide-module-content > ul.guide-toplevel > li.guide-section 
//   > div.guide-item-container > ul.guide-user-links > li#subscriptions-guide-item.guide-notification-item
//   > a.guide-item > (span.yt-valign-container > span.display-name) ~ (span.guide-count > span.guide-count-value)
// Without a guide count, the DOM is:
//  body#body > div#body-container > div#page-container > div#page > div#guide > div#appbar-guide-menu
//   > div#guide-container > div.guide-module-content > ul.guide-toplevel > li.guide-section
//   > div.guide-item-container > ul.guide-user-links > li#subscriptions-guide-item.guide-notification-item
//   > a.guide-item > span.yt-valign-container > span.display-name.no-count
// The diff seems to be:
//  in the context of #subscriptions-guide-item
//   remove .no-count from .display-name
//   append span.guide-count.yt-uix-tooltip.yt-valign to .guide-item
//   append span.yt-valign-container.guide-count-value to the new element
//   set its value to the desired number
// The same DOM exists on video pages (pop-up menu) as on feed pages (persistent menu)