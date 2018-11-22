const white = "#" + "FFFFFF";
const lightGrey = "#" + "D0D0D0";
const lightBlue = "#" + "B0B0D0";
const amp = "&";

var selectedRowIds = [];
var checkboxes = {};
var rows = {};

function q(){
  //console.log("Running function q");
  var oldScript = document.getElementById("fetch");
  var fromDate = get_val("fromDateText");
  var toDate = get_val("toDateText");
  //console.log("From date: " + fromDate);
  //console.log("To date: " + toDate);
  if(oldScript){
    oldScript.remove();
  }
  var s = document.createElement("script");
  s.src = "http://localhost:8080/?callback=load" + amp + "from="
    + fromDate
    + amp + "to="
    + toDate;
  s.id = "fetch";

  s.onload = function() {
    loadTable();
  };

  document.body.appendChild(s);

}

function loadTable(){
  var records = load();
  if(records.length == 0){
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
  var row = header.insertRow(0);
  var cell;
  var rec = records[0];

  var checkbox = document.createElement("INPUT");
  checkbox.setAttribute("type", "checkbox");
  checkbox.id = "allOrNone";
  checkbox.addEventListener("click", all_or_none_click);
  cell = row.insertCell(row.cells.length);
  cell.appendChild(checkbox);

  for (k in rec){
    if(k == "id"){
      continue;
    }
    cell = row.insertCell(row.cells.length);
    cell.innerHTML = "" + k;
    cell.bgColor = lightGrey;
  }

  records.forEach(tableWriter(table));
  document.body.appendChild(table);
}

function tableWriter(table){
  return function(obj){
           var row = table.insertRow();
           row.id = obj.id;
           rows[row.id] = row;
           var cell;
           var val;

           var checkbox = document.createElement("INPUT");
           checkbox.setAttribute("type", "checkbox");
           checkbox.id = "cbx" + row.id;
           checkboxes[row.id] = checkbox;
           checkbox.addEventListener("click", cbx_mouse_click);
           cell = row.insertCell(row.cells.length);
           cell.id = "cbx_cell_" + row.id;
           cell.appendChild(checkbox);

           for (var k in obj){
             val = obj[k];
             if(k == "id"){
               continue;
             } else if((k == "date" || k == "posted") && obj[k]){
               val = obj[k].split("T")[0];
             }
             cell = row.insertCell(row.cells.length);
             cell.id = "cell_" + k + "_" + row.id;
             cell.innerHTML = val;
             cell.addEventListener("mouseover", cell_mouse_over);
             cell.addEventListener("mouseout", cell_mouse_out);
             cell.addEventListener("click", cell_mouse_click);
           }

           row.style.cursor = "pointer";
         }
}

function get_val(Id){
  var element = document.getElementById(Id);
  return element.value;
}

function cell_mouse_over(event){
  var cell = event.target;
  //console.log("cell_mouse_over(" + event.target + ")")
  if(!is_row_selected(cell)){
    cell.parentElement.style.backgroundColor = lightGrey;
  }
}

function cell_mouse_out(event){
  var cell = event.target;
  //console.log("cell_mouse_out(" + event.target + ")")
  if(!is_row_selected(cell)){
    cell.parentElement.style.backgroundColor = white;
  }
}

function cell_mouse_click(event){
  var cell = event.target;
  //console.log("cell_mouse_click(" + event.target + ")")
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
  //console.log("Mouse over event target = " + cbx + " " + cbx.id);
  //console.log("Mouse over event target parent = " + parent + " " + parent.id);
  row_mouse_over({'target': parent});
}

function cbx_mouse_out(event){
  event.stopPropagation;
  var cbx = event.target;
  var parent = cbx.parentElement;
  //console.log("Mouse out event target = " + cbx + " " + cbx.id);
  //console.log("Mouse out event target parent = " + parent + " " + parent.id);
  row_mouse_out({'target': parent});
}

function is_row_selected(cell){
  var row = cell.parentElement;
  var rowId = row.id;
  //console.log("Is row " + rowId + "selected?");
  isSelected = selectedRowIds.findIndex(x => x == rowId) != -1;
  //console.log("Is row " + rowId + "selected? " + isSelected);
  return isSelected;
}

function is_any_row_selected(){
  isAnySelected = -1 != Object.values(checkboxes).findIndex(e => e.checked);
  //console.log("Is any selected: " + isAnySelected);
  return isAnySelected;
}

function select_row_by_cell(cell){
  var row = cell.parentElement;
  //console.log("select_row_by_cell(" + cell + ")");
  //console.log("select_row_by_cell parent = " + row);
  select_row_by_row(row);
}

function select_row_by_row(row){
  var rowId = row.id;
  select_row(rowId);
}

function select_row(rowId){
  selectedRowIds.push(rowId);
  checkboxes[rowId].checked = true;
  row = document.getElementById(rowId);
  row.style.backgroundColor = lightBlue;
  document.getElementById("allOrNone").checked = true;
}

function deselect_row_by_cell(cell){
  var row = cell.parentElement;
  deselect_row(row.id);
  //selectedRowIds = selectedRowIds.filter(x => x != rowId);
}

function deselect_row(rowId){
  selectedRowIds = selectedRowIds.filter(selRowId => selRowId != rowId);
  checkboxes[rowId].checked = false;
  row = document.getElementById(rowId);
  row.style.backgroundColor = white;
  if(!is_any_row_selected()){
    document.getElementById("allOrNone").checked = false;
  }
}

function all_or_none_click(event){
  if(is_any_row_selected()){
    //console.log("Some row(s) is/are selected");
    Object.values(checkboxes).forEach(cbx => cbx.checked = false);
    event.target.checked = false;
    selectedRowIds.forEach(row => deselect_row(row));
  }else{
    //console.log("No rows selected");
    Object.values(checkboxes).forEach(cbx => cbx.checked = true);
    event.target.checked = true;
    Object.values(rows).forEach(row => select_row_by_row(row));
  }
}

function getById(Id){
  return document.getElementById(Id);
}
