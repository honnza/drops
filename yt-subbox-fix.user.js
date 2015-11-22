// ==UserScript==
// @name          Youtube subbox number patch
// @description   replaces the (unreliable and since mid-Nov2015 broken) subscription feed new item counter in the menu
// @include       http*://youtube.com/*
// @version       0.0.1
// ==/UserScript==

if(location.pathname === "/feed/subscriptions"){
  GM_setValue("seenVids", JSON.stringify(scrapeIDs(document)))
} else {
	var seenVids = GM_getValue("seenVids");
  var xhr = new XmlHttpRequest();
  xhr.open("get", "/feed/subscriptions");
  xhr.onLoad = function(){
    var subVids = scrapeIDs(new DOMParser().parseFromString(xhr.response, "text/html"));
    var numUnseen = unseenVids.filter(id => ~ seenVids.indexOf(id)).length;
    var numElement = document.querySelector("#subscriptions-guide-item .guide-count-value")
    if(numElement){
    	numElement.textContent = numUnseen.toString();
    } else {
    	console.error("want to update the subscriptions guide count but no guide count value is already present");
    }
  };
  xhr.onError = console.error.bind(console);
  xhr.send();
}

function scrapeIDs(document){
  return [].map.call(document.querySelectorAll(".section-list .yt-lockup"), elem => elem.dataset.contextItemId)
}