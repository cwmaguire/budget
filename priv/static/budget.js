"use strict";
const white = "#" + "FFFFFF";
const lightGrey = "#" + "D0D0D0";
const lightBlue = "#" + "B0B0D0";
const amp = "&";

var selectedRowIds = [];
var checkboxes = {};
var rows = {};
var cats;

function fetch_transactions(){
  const scriptId = "transactionsScript";
  var oldScript = document.getElementById(scriptId);
  var fromDate = get_val("fromDateText");
  var toDate = get_val("toDateText");
  if(oldScript){
    oldScript.remove();
  }
  var s = document.createElement("script");
  s.src = "http://localhost:8080/" +
    "?callback=transactions" +
    "&from=" + fromDate +
    "&to=" + toDate;
  s.id = scriptId;

  s.onload = function() {
    load_transactions();
  };

  document.body.appendChild(s);
}

function fetch_categories(){
  const scriptId = "categoriesScript";
  var oldScript = document.getElementById(scriptId);
  if(oldScript){
    oldScript.remove();
  }
  var s = document.createElement("script");
  s.src = "http://localhost:8080/category" +
    "?callback=categories";
  s.id = scriptId;

  // TODO maybe enable the page when this is loaded?
  //s.onload = function() {
    //cats = categories();
  //};

  document.body.appendChild(s);
}

function load_transactions(){
  var txs = transactions();
  if(txs.length == 0){
    return;
  }

  checkboxes = {};

  var oldTable = document.getElementById("table1");
  if(oldTable){
    oldTable.remove();
  }
  var table = document.createElement("table");
  table.id = "table1";
  table.className = "table";

  var header = table.createTHead();
  var headerRow = header.insertRow(0);
  var cell;
  var tx = txs[0];

  var selectCheckbox = document.createElement("INPUT");
  selectCheckbox.setAttribute("type", "checkbox");
  selectCheckbox.id = "allOrNone";
  selectCheckbox.addEventListener("click", all_or_none_click);

  cell = headerRow.insertCell(headerRow.cells.length);
  cell.appendChild(selectCheckbox);

  for (let key in tx){
    if(key == "id"){
      continue;
    }
    cell = headerRow.insertCell(headerRow.cells.length);
    cell.innerHTML = "" + key;
    cell.bgColor = lightGrey;
  }

  cell = headerRow.insertCell(headerRow.cells.length);
  cell.innerHTML = "add category";
  cell.bgColor = lightGrey;

  txs.forEach(tableWriter(table));

  document.body.appendChild(table);
}

function tableWriter(table){
  return function(obj){
           let row = table.insertRow();
           row.id = obj.id;
           rows[row.id] = row;
           let cell;
           let val;

           let checkbox = document.createElement("INPUT");
           checkbox.setAttribute("type", "checkbox");
           checkbox.id = "cbx" + row.id;
           checkboxes[row.id] = checkbox;
           checkbox.addEventListener("click", cbx_mouse_click);
           cell = row.insertCell(row.cells.length);
           cell.id = "cbx_cell_" + row.id;
           cell.appendChild(checkbox);

           for(var k in obj){
             val = obj[k];
             if(k == "id"){
               continue;
             }else if((k == "date" || k == "posted") && obj[k]){
               val = obj[k].split("T")[0];
             }
             cell = row.insertCell(row.cells.length);
             cell.id = "cell_" + k + "_" + row.id;
             cell.innerHTML = val;
             cell.addEventListener("mouseover", cell_mouse_over);
             cell.addEventListener("mouseout", cell_mouse_out);
             cell.addEventListener("click", cell_mouse_click);
           }

           let datalistId = "cat_dl_" + row.id;

           let categoryDatalist = document.createElement("DATALIST");
           categoryDatalist.id = datalistId;
           add_categories_to_datalist(categoryDatalist);

           let categoryInput = document.createElement("INPUT");
           categoryInput.id = "cat_input_" + row.id;
           categoryInput.setAttribute('list', datalistId);

           let categoryButton = document.createElement("INPUT");
           categoryButton.id = "cat_add_button_" + row.id;
           categoryButton.value = "+";
           categoryButton.type = "button";
           categoryButton.addEventListener("click", cat_add_click);

           cell = row.insertCell(row.cells.length);
           cell.id = "cat_dl_cell_" + row.id;
           cell.appendChild(categoryInput);
           cell.appendChild(categoryDatalist);
           cell.appendChild(categoryButton);

           row.style.cursor = "pointer";
         }
}

function add_categories_to_datalist(datalist){
  //console.log("Adding " + categories().length + " categories to datalist " + datalist.id);
  categories().forEach(x => insert_category(datalist, x));
}

function insert_category(datalist, category){
  //console.log("Adding category " + category.name + " to datalist " + datalist.id);
  var option = document.createElement("OPTION");
  option.id = datalist.id + "_option_cat_" + category['id'];
  option.value = category['name'];
  datalist.appendChild(option);
}

function get_val(Id){
  var element = document.getElementById(Id);
  return element.value;
}

function cell_mouse_over(event){
  var cell = event.target;
  if(!is_row_selected(cell)){
    cell.parentElement.style.backgroundColor = lightGrey;
  }
}

function cell_mouse_out(event){
  var cell = event.target;
  if(!is_row_selected(cell)){
    cell.parentElement.style.backgroundColor = white;
  }
}

function cell_mouse_click(event){
  var cell = event.target;
  if(is_row_selected(cell)){
    deselect_row_by_cell(cell);
  }else{
    select_row_by_cell(cell);
  }
}

function cbx_mouse_click(event){
  var cbx = event.target;
  var newEvent = {'target': cbx.parentElement};
  // don't let the row handle the click
  event.stopPropagation();
  cell_mouse_click(newEvent);
}

function cbx_mouse_over(event){
  event.stopPropagation;
  event.preventDefault;
  var cbx = event.target;
  var parent = cbx.parentElement;
  row_mouse_over({'target': parent});
}

function cbx_mouse_out(event){
  event.stopPropagation;
  var cbx = event.target;
  var parent = cbx.parentElement;
  row_mouse_out({'target': parent});
}

function is_row_selected(cell){
  var row = cell.parentElement;
  return -1 != selectedRowIds.findIndex(x => x == row.id);
}

function is_any_row_selected(){
  return -1 != Object.values(checkboxes).findIndex(e => e.checked);
}

function select_row_by_cell(cell){
  select_row_by_row(cell.parentElement);
}

function select_row_by_row(row){
  select_row(row.id);
}

function select_row(rowId){
  selectedRowIds.push(rowId);
  checkboxes[rowId].checked = true;
  let row = document.getElementById(rowId);
  row.style.backgroundColor = lightBlue;
  document.getElementById("allOrNone").checked = true;
}

function deselect_row_by_cell(cell){
  deselect_row(cell.parentElement.id);
}

function deselect_row(rowId){
  selectedRowIds = selectedRowIds.filter(selRowId => selRowId != rowId);
  checkboxes[rowId].checked = false;
  let row = document.getElementById(rowId);
  row.style.backgroundColor = white;
  if(!is_any_row_selected()){
    document.getElementById("allOrNone").checked = false;
  }
}

function all_or_none_click(event){
  if(is_any_row_selected()){
    Object.values(checkboxes).forEach(cbx => cbx.checked = false);
    event.target.checked = false;
    selectedRowIds.forEach(row => deselect_row(row));
  }else{
    Object.values(checkboxes).forEach(cbx => cbx.checked = true);
    event.target.checked = true;
    Object.values(rows).forEach(row => select_row_by_row(row));
  }
}

function cat_add_click(event){
  let rowId = event.target.id.split("_")[3];
  let value = document.getElementById("cat_input_" + rowId).value;
  let categoryId = categoryIdByValue(categories(), value);
  if(-1 != categoryId){
    console.log("Category ID = " + categoryId);
    post("category", "tx=" + rowId + "&cat=" + categoryId);
  }else{
    console.log("No category ID");
  }
}

function getById(Id){
  return document.getElementById(Id);
}

function categoryIdByValue(categories, value){
  let iterator = categories.entries();
  let key = "name";
  let entry = iterator_find_first(iterator, key, value);
  if(entry == -1){
    return -1;
  }else{
    return entry[0];
  }
}

function iterator_find_first(iterator, key, value){
  let n = iterator.next();
  do {
    if(n.value[1][key] === value){
      return n.value;
    }else{
      n = iterator.next();
    }
  } while(!n.done);
  return -1;
}

function add_category(){

}

function post(restPath, postKVs){
  var xhr = new XMLHttpRequest();

  console.log("Calling post with postKVs: " + postKVs);
  xhr.addEventListener("progress", updateProgress);
  xhr.addEventListener("load", transferComplete);
  xhr.addEventListener("error", transferFailed);
  xhr.addEventListener("abort", transferCanceled);
  xhr.open("POST", "http://localhost:8080/" + restPath, true);
  xhr.setRequestHeader("Content-type", "application/x-www-form-urlencoded")
  xhr.send(postKVs);
}

// progress on transfers from the server to the client (downloads)
function updateProgress (oEvent) {
  if (oEvent.lengthComputable) {
    var percentComplete = oEvent.loaded / oEvent.total * 100;
    console.log("update progress");
  } else {
    // Unable to compute progress information since the total size is unknown
    console.log("update progress: total size unknown");
  }
}

function transferComplete(evt) {
  console.log("The transfer is complete.");
}

function transferFailed(evt) {
  console.log("An error occurred while transferring the file.");
}

function transferCanceled(evt) {
  console.log("The transfer has been canceled by the user.");
}

