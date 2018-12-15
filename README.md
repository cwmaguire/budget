# budget
"budget" is a budgeting web app to answer the question: "How much have I spent?"

I want to be able to download my transactions, import them and the categorize them based on patterns that I specify.

### Goals
- Graph spending according to cagtegories
- Categorize spending, automate as much as possible
- Split transactions so they can go into multiple cateogories
- Import transactions CSV spreadsheets

### Features:
- parse CSVs from one institution
- fetch transactions
- render transactions in a table
- add/delete transaction categories
- "split" transactions
  - add/delete child transactions
  - enable/disable editing of parent transactions
- editable CAD and USD fields
- editable notes field
- Import RBC transactions from web
- Add preset dates, set "to date" to way in the future

### Todo:
- delete multiple transactions
- add categories to multiple transactions
- auto-category rule editor
  - apply new rules to existing transactions
- parse transactions from another institution.
- draw graphs
- add column sorting
  - For now I can just sort by date descending, I want to see what I've spent most recently
- Add interface to add/edit categories

### Tools
- Erlang
-- Cowboy web server
- JavaScript
- HTML
- CSS
