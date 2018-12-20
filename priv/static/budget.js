"use strict";

// DEPENDS ON budget_http.js

// TODO I think I can remove these now that I have the JS in its own file
// I think it was just the HTML filetype in Vim that struggled with HTML
// and JS combined.
const white = "#" + "FFFFFF";
const lightGrey = "#" + "D0D0D0";
const lightBlue = "#" + "B0B0D0";
const amp = "&";

// TODO flag automatic vs. manual categories
// It would be nice to know which categories I added and which categories
// were added by rules.

// TODO Add notes
// It's helpful to know a little more detail about obscure transactions

let selectedRowIds = [];
let checkboxes = {};
let rows = {};
let cats;

// fetch button onclick handler
function fetch_transactions(){
  const scriptId = "transactionsScript";
  let oldScript = elem_by_id(scriptId);
  let fromDate = get_val("fromDateText");
  let toDate = get_val("toDateText");
  if(oldScript){
    oldScript.remove();
  }
  let s = document.createElement("script");
  s.src = "http://localhost:8080/transaction/" +
    "?callback=transactions" +
    "&from=" + fromDate +
    "&to=" + toDate;
  s.id = scriptId;

  s.onload = function() {
    load_transactions();
  };

  document.body.appendChild(s);
}

// body.onload handler
function fetch_categories(){
  const scriptId = "categoriesScript";
  let oldScript = elem_by_id(scriptId);
  if(oldScript){
    oldScript.remove();
  }
  let s = document.createElement("script");
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
  let txs = transactions();
  if(txs.length == 0){
    return;
  }

  checkboxes = {};

  let oldTable = elem_by_id("table1");
  if(oldTable){
    oldTable.remove();
  }
  let table = document.createElement("table");
  table.id = "table1";
  table.className = "table";

  let header = table.createTHead();
  let headerRow = header.insertRow(0);
  let cell;
  let tx = txs[0];

  let selectCheckbox = document.createElement("INPUT");
  selectCheckbox.setAttribute("type", "checkbox");
  selectCheckbox.id = "allOrNone";
  selectCheckbox.addEventListener("click", all_or_none_click);

  cell = headerRow.insertCell(headerRow.cells.length);
  cell.appendChild(selectCheckbox);

  for (let key in tx){
    if(key == "id" ||
      key == "acct_num" ||
      key == "parent" ||
      key == "child_number" ||
      key == "is_parent" ||
      key == "note"){
      continue;
    }
    cell = headerRow.insertCell(headerRow.cells.length);
    cell.innerHTML = "" + key;
    cell.bgColor = lightGrey;
  }

  let extraColumns = ["add_category",
                      "delete",
                      "split",
                      "note"];
  extraColumns.forEach(
    function(title){
      cell = headerRow.insertCell(headerRow.cells.length);
      cell.innerHTML = title;
      cell.bgColor = lightGrey;
    });

  let tbody = table.createTBody();

  txs.forEach(table_writer(tbody));

  document.body.appendChild(table);
}

function table_writer(tbody){
  return function(obj){ create_tx_row(tbody, obj, 'last'); };
}

function create_tx_row(tbody, obj, pos){
  let index;
  let cell;
  let val;
  let cats;
  let splitEventHandler
  let categorySpans;
  let isParent = Boolean(obj.is_parent);
  let isChild = Boolean(obj.parent);

  if(pos == 'last'){
    index = -1;
  }else{
    index = pos;
  }
  let row = tbody.insertRow(index);
  row.id = obj.id;
  rows[row.id] = row;
  row.style.cursor = "pointer";

  if(isParent){
    row.className = "parent";
  }else if(isChild){
    row.className = "child";
  }

  let checkbox = document.createElement("INPUT");
  checkbox.setAttribute("type", "checkbox");
  checkbox.id = "cbx" + row.id;
  checkboxes[row.id] = checkbox;
  checkbox.addEventListener("click", cbx_mouse_click);
  cell = row.insertCell(row.cells.length);
  cell.id = "cbx_cell_" + row.id;
  cell.appendChild(checkbox);

  for(let k in obj){
    val = obj[k];
    if(k == "id" || k == "note" || k == "acct_num"){
      continue;
    }else if(k == "categories"){
      cell = row.insertCell(row.cells.length);
      cell.id = "cell_" + k + "_" + row.id;

      if(!isParent){
        categorySpans = [];
        if(obj[k]){
          cats = obj[k].split(", ");
          cats.forEach(
            function(cat){
              categorySpans.push(cat_span(cat))
            }
          );
        }
        categorySpans.forEach(
          function(span){
            cell.appendChild(span);
          }
        );
      }
      continue;
    }else if((k == "date" || k == "posted") && obj[k]){
      val = obj[k].split("T")[0];
    }else if(k == "parent" ||
      k == "child_number" ||
      k == "is_parent"){
      continue;
    } else if((k == "cad" || k == "usd") && isChild){
      cell = row.insertCell(row.cells.length);
      cell.id = "cell_" + k + "_" + row.id;

      let amountText = document.createElement("INPUT");
      amountText.setAttribute("type", "text");
      amountText.value = val;
      amountText.className = "dollarAmount";
      amountText.addEventListener("change", amount_text_change);
      cell.appendChild(amountText);
      continue;
    }
    cell = row.insertCell(row.cells.length);
    cell.id = "cell_" + k + "_" + row.id;
    cell.innerHTML = val;
    cell.addEventListener("click", cell_mouse_click);
  }

  // Category cell (or empty if parent)
  cell = row.insertCell(row.cells.length);
  cell.id = "cat_dl_cell_" + row.id;

  if(!isParent){
    add_category_controls(cell, row.id);
  }

  cell = row.insertCell(row.cells.length);
  cell.id = "tx_delete_cell_" + row.id;

  if(!isParent){
    add_delete_button(cell, row.id);
  }

  // Split Button (or blank for child rows)
  cell = row.insertCell(row.cells.length);
  cell.id = "tx_split_cell_" + row.id;
  if(isParent || !isChild){
    let splitButton = document.createElement("INPUT");
    if(isParent){
      splitButton.value = "split again";
      splitButton.addEventListener("click", tx_split_add_click);
    }else if(!isChild){
      splitButton.value = "split";
      splitButton.addEventListener("click", tx_split_click);
    }
    splitButton.id = "tx_split_button_" + row.id;
    splitButton.type = "button";

    cell.appendChild(splitButton);
  }

  cell = row.insertCell(row.cells.length);
  cell.id = "tx_note_cell_" + row.id;

  let noteText = document.createElement("INPUT");
  noteText.setAttribute("type", "text");
  noteText.value = obj.note;
  noteText.addEventListener("change", note_text_change);
  cell.appendChild(noteText);
}

function add_category_controls(cell, tx_id){
  let datalistId = "cat_dl_" + tx_id;

  let categoryDatalist = document.createElement("DATALIST");
  categoryDatalist.id = datalistId;
  add_categories_to_datalist(categoryDatalist);

  let categoryInput = document.createElement("INPUT");
  categoryInput.id = "cat_input_" + tx_id;
  categoryInput.setAttribute('list', datalistId);

  let categoryButton = document.createElement("INPUT");
  categoryButton.id = "cat_add_button_" + tx_id;
  categoryButton.value = "+";
  categoryButton.type = "button";
  categoryButton.addEventListener("click", cat_add_click);

  cell.appendChild(categoryInput);
  cell.appendChild(categoryDatalist);
  cell.appendChild(categoryButton);
}

function add_delete_button(cell, tx_id){
  let deleteButton = document.createElement("INPUT");
  //deleteButton.value = "delete";
  deleteButton.id = "tx_delete_button_" + tx_id;
  deleteButton.type = "button";
  deleteButton.className = "delete";
  deleteButton.addEventListener("click", tx_delete_click);
  cell.appendChild(deleteButton);
}

function add_categories_to_datalist(datalist){
  categories().forEach(x => insert_category(datalist, x));
}

function insert_category(datalist, category){
  let option = document.createElement("OPTION");
  option.id = datalist.id + "_option_cat_" + category['id'];
  option.value = category['name'];
  datalist.appendChild(option);
}

function get_val(Id){
  let element = elem_by_id(Id);
  return element.value;
}

function cell_mouse_over(event){
  let cell = event.target;
  if(!is_row_selected(cell)){
    cell.parentElement.style.backgroundColor = lightGrey;
  }
}

function cell_mouse_out(event){
  let cell = event.target;
  if(!is_row_selected(cell)){
    cell.parentElement.style.backgroundColor = white;
  }
}

function cell_mouse_click(event){
  let cell = event.target;
  if(is_row_selected(cell)){
    deselect_row_by_cell(cell);
  }else{
    select_row_by_cell(cell);
  }
}

function cbx_mouse_click(event){
  let cbx = event.target;
  let newEvent = {'target': cbx.parentElement};
  // don't let the row handle the click
  event.stopPropagation();
  cell_mouse_click(newEvent);
}

function cbx_mouse_over(event){
  event.stopPropagation;
  event.preventDefault;
  let cbx = event.target;
  let parent = cbx.parentElement;
  row_mouse_over({'target': parent});
}

function cbx_mouse_out(event){
  event.stopPropagation;
  let cbx = event.target;
  let parent = cbx.parentElement;
  row_mouse_out({'target': parent});
}

function is_row_selected(cell){
  let row = cell.parentElement;
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
  let row = elem_by_id(rowId);
  row.className = row.className + "Selected";
  elem_by_id("allOrNone").checked = true;
}

function deselect_row_by_cell(cell){
  deselect_row(cell.parentElement.id);
}

function deselect_row(rowId){
  selectedRowIds = selectedRowIds.filter(selRowId => selRowId != rowId);
  checkboxes[rowId].checked = false;
  let row = elem_by_id(rowId);
  if(row.className == "parentSelected"){
    row.className = "parent";
  }else if(row.className == "childSelected"){
    row.className = "child";
  }
  if(!is_any_row_selected()){
    elem_by_id("allOrNone").checked = false;
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
  let input = elem_by_id("cat_input_" + rowId);
  let value = input.value;
  let categoryId = category_id_by_value(categories(), value);
  if(-1 != categoryId){
    let result = http_post("transaction_category/",
                           "tx=" + rowId + "&cat=" + categoryId,
                           function (){ reset_categories(rowId) });
    reset_categories(rowId);
  }
  input.value = "";
}

function reset_categories(rowId){
  let cell = elem_by_id("cell_categories_" + rowId);
  http_get("transaction_category?tx=" + rowId,
           function (text){
             update_cell(cell, text);
           });
}

function update_cell(cell, text){
  let categorySpans = [];

  for(let i = 0; i < cell.children.length; i++){
    cell.firstElementChild.remove();
  }

  let cats = text.split(", ");

  cats.forEach(
    function(cat){
      cell.appendChild(cat_span(cat))
    }
  );
}

function elem_by_id(Id){
  return document.getElementById(Id);
}

function category_id_by_value(cats, value){
  let iterator = cats.entries();
  let key = "name";
  let entry = iterator_find_first(iterator, key, value);
  if(entry == -1){
    return -1;
  }else{
    return entry[1].id;
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

// TODO what was this for?
function add_category(){

}

function cat_span(cat){
  let span = document.createElement("SPAN");
  let descAndId = cat.split("||");
  span.innerHTML = descAndId[0];
  span.id = descAndId[1];
  span.classList.add("category");
  span.addEventListener("click", category_click);
  return span;
}

function category_click(event){
  let catSpan = event.target;
  delete_category(catSpan);
}

function tx_split_click(event){
  let row = event.target.parentElement.parentElement;
  add_tx_children(event, 2);
  event.target.value = "add split";
  event.target.removeEventListener("click", tx_split_click);
  event.target.onclick = tx_split_add_click;
  clear_categories(row);
  disable_row(row);
  row.className = "parent";
}

function tx_split_add_click(event){
  add_tx_children(event, 1);
}

function tx_delete_click(event){
  let row = event.target.parentElement.parentElement;
  http_delete(
    "transaction/" + row.id,
    function(parent){
      del_tx_child(row, parent)
    });
}

function add_tx_children(event, count){
  let button = event.target;
  let row = button.parentElement.parentElement;
  let txId = button.id.split("_")[3];
  for(let i = 0; i < count; i++){
    let kvs = "tx_id=" + txId + "&callback=transactions" + i
    http_post("transaction", kvs, callback_function(row, i));
  }
}

function del_tx_child(row, parent){
  row.remove();
  if(parent){
    if_not_has_children(
      parent,
      function(){
        unparent_row(parent)
      });
  }
}

function if_not_has_children(parent, fun){
  http_get("transaction?parent=" + parent + "&callback=child_counts",
           function (jsonp){
             use_script(
               jsonp,
               function(){
                 if(child_counts()[0].count < 1){
                   fun();
                 }
               });
           });
}

function use_script(jsonp, fun){
  let s = document.createElement("SCRIPT");
  s.text = jsonp;
  document.body.appendChild(s);
  fun();
  s.remove();
}

function unparent_row(tx_id){
  let row = elem_by_id(tx_id);
  row.className = "";
  let splitButton = elem_by_id("tx_split_button_" + tx_id);
  splitButton.value = "split";
  splitButton.removeEventListener("click", tx_split_add_click);
  splitButton.onclick = tx_split_click;
  let catCell = elem_by_id("cat_dl_cell_" + tx_id);
  add_category_controls(catCell, tx_id);
  let deleteCell = elem_by_id("tx_delete_cell_" + tx_id);
  add_delete_button(deleteCell, tx_id);
}

function callback_function(row, i){
  let f = function(jsonp){
    use_script(
      jsonp,
      function(){
            add_transaction_row(row, window["transactions" + i]());
      });
  };
  return f;
}

function add_transaction_row(parentRow, objs){
  let obj = objs[0];
  let table = elem_by_id("table1");
  let tbody = table.tBodies[0];
  let index = parentRow.rowIndex;

  create_tx_row(tbody, obj, index);
}

function clear_categories(row){
  let catSpan;
  let categoriesCell = elem_by_id("cell_categories_" + row.id);
  for(let i = 0; i < categoriesCell.children.length; i++){
    delete_category(categoriesCell.children[i]);
  }
}

function delete_category(catSpan){
  http_delete("transaction_category/" + catSpan.id,
              function(){
                elem_by_id(catSpan.id).remove()
              });
}

function disable_row(row){
  [11,12].forEach(
    function(i){
      row.cells[i].childNodes.forEach(function (e){ e.remove(); });
    });
}

function amount_text_change(event){
  let row = event.target.parentElement.parentElement;
  let cadText = row.cells[6].childNodes[0];
  let cad = cadText.value;
  let usdText = row.cells[7].childNodes[0];
  let usd = usdText.value;
  let isValid = true;

  if(cad.length > 0 && !is_float(cad)){
    cadText.className = "invalid";
    isValid = false;
  }else{
    cadText.className = "dollarAmount";
  }

  if(usd.length > 0 && !is_float(usd)){
    usdText.className = "invalid";
    isValid = false;
  }else{
    usdText.className = "dollarAmount";
  }

  if(isValid){
    http_put(
      "transaction/" + row.id,
      "cad=" + add_decimal(cad) + "&usd=" + add_decimal(usd),
      function(){}
    );
  }
}

function is_float(maybeFloat){
  return !isNaN(parseFloat(maybeFloat)) &&
    maybeFloat.match(/^-?\d+(\.(?=\d)\d*$)?$/);
}

function add_decimal(number){
  if(number.length == 0){
    return number;
  }else if(number.indexOf(".") == -1){
    return number + ".0";
  }else{
    return number;
  }
}

function note_text_change(event){
  let elem = event.target;
  let cell = elem.parentElement;
  let row = cell.parentElement;
  let noteText = elem.value;

  http_put(
    "transaction/" + row.id,
    "note=" + noteText,
    function(){}
  );
}

function upload_file(event){
  let file = elem_by_id("uploadFile").files[0];
  let type = elem_by_id("csvType").value;
  http_post_file("transaction/?type=" + type, file, upload_complete);
}

function upload_complete(event){
  console.log(event);
}

function preset_from_date_change(event){
  let presetFromDate = event.target;
  let interval = presetFromDate.value;
  let fromDateText = elem_by_id('fromDateText');
  let now = new Date();
  let presetDate = subtract_interval(now, interval);
  fromDateText.value = format_date(presetDate);
  presetFromDate.value = "";
}

function subtract_interval(date, interval){
  let dateMillis = date.getTime();
  let deltaMillis = -1;
  switch(interval) {
    case "1 week ago":
      deltaMillis = milliseconds_delta("1 week");
      return new Date(dateMillis - deltaMillis);
      break;
    case "2 weeks ago":
      deltaMillis = milliseconds_delta("2 weeks");
      return new Date(dateMillis - deltaMillis);
      break;
    case "3 weeks ago":
      deltaMillis = milliseconds_delta("3 weeks");
      return new Date(dateMillis - deltaMillis);
      break;
    case "4 weeks ago":
      deltaMillis = milliseconds_delta("4 weeks");
      return new Date(dateMillis - deltaMillis);
      break;
    case "6 weeks ago":
      deltaMillis = milliseconds_delta("6 weeks");
      return new Date(dateMillis - deltaMillis);
      break;
    case "1 month ago":
      deltaMillis = milliseconds_delta("1 month");
      return new Date(dateMillis - deltaMillis);
      break;
    case "2 months ago":
      deltaMillis = milliseconds_delta("2 months");
      return new Date(dateMillis - deltaMillis);
      break;
  }
}

function milliseconds_delta(interval){
  const MINUTE_MILLIS = 60000;
  const HOUR_MILLIS = MINUTE_MILLIS * 60;
  const DAY_MILLIS = HOUR_MILLIS * 24;
  const WEEK_MILLIS = DAY_MILLIS * 7;
  const MONTH_MILLIS = DAY_MILLIS * 30;
  switch(interval){
    case "1 week":
      return WEEK_MILLIS;
      break;
    case "2 weeks":
      return 2 * WEEK_MILLIS;
      break;
    case "3 weeks":
      return 3 * WEEK_MILLIS;
      break;
    case "4 weeks":
      return 4 * WEEK_MILLIS;
      break;
    case "6 weeks":
      return 6 * WEEK_MILLIS;
      break;
    case "6 weeks":
      return 6 * WEEK_MILLIS;
      break;
    case "1 month":
      return 1 * MONTH_MILLIS;
      break;
    case "2 months":
      return 2 * MONTH_MILLIS;
      break;
  }
}

function format_date(date1){
  let day = date1.getDate();
  let month = date1.getMonth() + 1;
  let year = date1.getFullYear();
  return year + "-" + month + "-" + (day < 10 ? "0" + day : day);
}
