"use strict";

function show_add_dialog(){
  let newDialog = elem_by_id('newDialog');
  newDialog.style.visibility = 'visible';
}

function add_new(event){

  let acctType = elem_by_id('newAcct').value;
  let date = elem_by_id('newDate').value;
  let desc = elem_by_id('newDesc').value;
  let cad = elem_by_id('newCad').value;
  let usd = elem_by_id('newUsd').value;
  let note = elem_by_id('newNote').value;
  let kvs =
    "acct_type=" + acctType +
    "&date=" + date +
    "&desc_1=" + desc +
    "&cad=" + cad +
    "&usd=" + usd +
    "&note=" + note;
  http_post("transaction", kvs, close_dialog);
}

function cancel_new(event){
  event.target.parentElement.style.visibility = 'hidden';
}

function setup_new_dialog(){
  elem_by_id('newDate').value = format_date(new Date());

  let newDialog = elem_by_id('showAddDialog');
  newDialog.addEventListener('click', show_add_dialog);

  let newButton = elem_by_id('newAddButton');
  newButton.addEventListener('click', add_new);

  let cancelButton = elem_by_id('newCancelButton');
  cancelButton.addEventListener('click', cancel_new);
}

function close_dialog(){
  elem_by_id('newAcct').value = 'cash';
  elem_by_id('newDate').value = format_date(new Date());
  elem_by_id('newDesc').value = '';
  elem_by_id('newCad').value = '';
  elem_by_id('newUsd').value = '';
  elem_by_id('newNote').value = '';
  elem_by_id('newDialog').style.visibility = 'hidden';
}
