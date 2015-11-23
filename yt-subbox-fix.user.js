// ==UserScript==
// @name          Youtube subbox number patch
// @description   replaces the (unreliable and since mid-Nov2015 broken) subscription feed new item counter in the menu
// @include       http*://*.youtube.com/*
// @version       0.0.15
// ==/UserScript==

if(location.pathname === "/feed/subscriptions"){
  GM_setValue("seenVids", JSON.stringify(scrapeIDs(document)));
  console.log("uptdated the list of seen vids");
} else {
	var seenVids = JSON.parse(GM_getValue("seenVids"));
  var xhr = new XMLHttpRequest();
  xhr.open("get", "/feed/subscriptions");
  xhr.onload = function(){
    var subVids = scrapeIDs(new DOMParser().parseFromString(xhr.response, "text/html"));
    var numUnseen = subVids.filter(id => !~seenVids.indexOf(id)).length;
    awaitElement(document.getElementById("guide"), ".guide-count-value", function(numElement){
    	numElement.textContent = numUnseen.toString();
    	console.log("successfully updated the subscriptions guide count")
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
		console.log("awaited element exists");
		callback(node);
	}else{
		console.log("awaiting element")
		mo = new MutationObserver(function(records){
			records.forEach(function(record){
				[].some.call(record.addedNodes, function(node){
					if(node.nodeType === node.ELEMENT_NODE && node.matches(selector)){
						callback(node);
						mo.unobserve();
						return true;
					}
					return false;
				});
			});
		});
		mo.observe(parent, {childList:true, subtree:true})
	}
}