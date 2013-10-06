// ==UserScript==
// @name          Stack Exchange chat sticky flags fix
// @description   Fix for the flag bubble sometimes staying up after flags have been dismissed.
// @description   Hides the flag bubble if a "no message" popup is shown in response to a bubble click.
// @include       http://stackexchange.com/questions?tab=realtime
// @version       1.0
// ==/UserScript==

(function(w){
  $(document).on('ajaxComplete', function(ev, xhr, opts){
    if(/\/admin\/flags/.test(opts.url)){
      var r = JSON.parse(xhr.responseText);
      if(!r.messages || !r.messages.length){
        $("#flag-count").remove();
      }
    }
  })
}(window))