"use strict";

function show_add_dialog(){
  let newDialog = elem_by_id('newDialog');
  newDialog.style.visibility = 'visible';
}

function add_new(event){

}

function cancel_new(event){
  event.target.parentElement.style.visibility = 'hidden';
}

function addNewEventListeners(){
  let newDialog = elem_by_id('showAddDialog');
  newDialog.addEventListener('click', show_add_dialog);

  let newButton = elem_by_id('newAddButton');
  newButton.addEventListener('click', add_new);

  let cancelButton = elem_by_id('newCancelButton');
  cancelButton.addEventListener('click', cancel_new);
}
