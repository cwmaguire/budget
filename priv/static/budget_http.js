"use strict";

function http_get(restPath, callback){
  var xhr = new XMLHttpRequest();

  xhr.addEventListener("progress", update_progress);
  xhr.addEventListener("load", transfer_complete);
  xhr.addEventListener("error", transfer_failed);
  xhr.addEventListener("abort", transfer_canceled);
  xhr.addEventListener("readystatechange", http_ready_state_change_event_handler(xhr, callback));
  xhr.open("GET", "http://localhost:8080/" + restPath, true);
  xhr.send();
}

function http_post(restPath, postKVs, callback){
  var xhr = new XMLHttpRequest();

  xhr.addEventListener("progress", update_progress);
  xhr.addEventListener("load", transfer_complete);
  xhr.addEventListener("error", transfer_failed);
  xhr.addEventListener("abort", transfer_canceled);
  xhr.addEventListener("readystatechange", http_ready_state_change_event_handler(xhr, callback));
  xhr.open("POST", "http://localhost:8080/" + restPath, true);
  xhr.setRequestHeader("Content-type", "application/x-www-form-urlencoded")
  xhr.send(postKVs);
}

function http_post_file(restPath, file, callback){
  var xhr = new XMLHttpRequest();

  xhr.addEventListener("progress", update_progress);
  xhr.addEventListener("load", transfer_complete);
  xhr.addEventListener("error", transfer_failed);
  xhr.addEventListener("abort", transfer_canceled);
  xhr.addEventListener("readystatechange", http_ready_state_change_event_handler(xhr, callback));
  xhr.open("POST", "http://localhost:8080/" + restPath, true);
  xhr.setRequestHeader("Content-type", "text/csv")
  xhr.send(file);
}

function http_put(restPath, putKVs, callback){
  var xhr = new XMLHttpRequest();

  xhr.addEventListener("progress", update_progress);
  xhr.addEventListener("load", transfer_complete);
  xhr.addEventListener("error", transfer_failed);
  xhr.addEventListener("abort", transfer_canceled);
  xhr.addEventListener("readystatechange", http_ready_state_change_event_handler(xhr, callback));
  xhr.open("PUT", "http://localhost:8080/" + restPath, true);
  xhr.setRequestHeader("Content-type", "application/x-www-form-urlencoded")
  xhr.send(putKVs);
}

function http_delete(restPath, callback){
  var xhr = new XMLHttpRequest();

  xhr.addEventListener("progress", update_progress);
  xhr.addEventListener("load", transfer_complete);
  xhr.addEventListener("error", transfer_failed);
  xhr.addEventListener("abort", transfer_canceled);
  xhr.addEventListener("readystatechange", http_ready_state_change_event_handler(xhr, callback));
  xhr.open("DELETE", "http://localhost:8080/" + restPath, true);
  xhr.send();
}

function http_ready_state_change_event_handler(xhr, callback){
  let handler = function http_get_ready_state_change(event){
    if(xhr.readyState == 4 && xhr.status < 300){
      callback(xhr.responseText);
    }
  }
  return handler;
}

// progress on transfers from the server to the client (downloads)
function update_progress (oEvent) {
  if (oEvent.lengthComputable) {
    var percentComplete = oEvent.loaded / oEvent.total * 100;
  } else {
  }
}

function transfer_complete(evt) {
}

function transfer_failed(evt) {
}

function transfer_canceled(evt) {
}
