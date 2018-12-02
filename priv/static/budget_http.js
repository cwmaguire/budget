"use strict";

function http_post(restPath, postKVs, callback){
  var xhr = new XMLHttpRequest();

  console.log("Calling http_post with postKVs: " + postKVs);
  xhr.addEventListener("progress", update_progress);
  xhr.addEventListener("load", transfer_complete);
  xhr.addEventListener("error", transfer_failed);
  xhr.addEventListener("abort", transfer_canceled);
  xhr.addEventListener("readystatechange", http_ready_state_change_event_handler(xhr, callback));
  xhr.open("POST", "http://localhost:8080/" + restPath, true);
  xhr.setRequestHeader("Content-type", "application/x-www-form-urlencoded")
  xhr.send(postKVs);
  //console.log("xhr.responseText = " + xhr.responseText);
  //console.log("xhr.response = " + xhr.response);
}

function http_post_ready_state_change_event_handler(xhr, callback){
  let handler = function http_get_ready_state_change(event){
    if(xhr.readyState == 4){
      //console.log("ready state == 4: xhr.responseText = " + xhr.responseText);
      //console.log("ready state == 4: xhr.response = " + xhr.response);
      callback(xhr.responseText);
    }
  }
  return handler;
}

function http_get(restPath, callback){
  var xhr = new XMLHttpRequest();

  //console.log("Calling http_get with restPath: " + restPath);
  xhr.addEventListener("progress", update_progress);
  xhr.addEventListener("load", transfer_complete);
  xhr.addEventListener("error", transfer_failed);
  xhr.addEventListener("abort", transfer_canceled);
  xhr.addEventListener("readystatechange", http_ready_state_change_event_handler(xhr, callback));
  xhr.open("GET", "http://localhost:8080/" + restPath, true);
  xhr.send();
}

function http_delete(restPath, callback){
  var xhr = new XMLHttpRequest();

  //console.log("Calling http_delete with restPath: " + restPath);
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
    //console.log("xhr.status = " + xhr.status);
    //console.log("xhr.statusText = " + xhr.statusText);
    if(xhr.readyState == 4 && xhr.status < 300){
      //console.log("xhr.readyState = " + xhr.readyState);
      //console.log("xhr.responseText = " + xhr.responseText);
      //console.log("xhr.response = " + xhr.response);
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
  console.log("An error occurred while transferring the file.");
}

function transfer_canceled(evt) {
  console.log("The transfer has been canceled by the user.");
}
