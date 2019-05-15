#lang pollen

function handle_expander_click(id) {
  var feature_box = document.getElementById(id);
  feature_box.classList.toggle("active_expander");
}

function cancel_bubble(e) {
  // prevents click on hyperlink from propagating into clickable div behind it
  var evt = e ? e:window.event;
  if (evt.stopPropagation) {
    evt.stopPropagation();
  }
  if (evt.cancelBubble) { 
    evt.cancelBubble = true;
  }
}
