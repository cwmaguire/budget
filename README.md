# budget
Code to import banking spreadsheets and categorize transactions

I need to check my budget everyday but Mint isn't cutting it. I want to
be able to download my transactions, import them and the categorize them
based on patterns that I input. The transactions will go into a database
table so I can query for transactions by category and date. The category
will be handled by a separate query so that a single transaction can be
in multiple categories.

### Features:
- parse CSVs from one institution
- fetch transactions
- render transactions in a table
- add/delete transaction categories
- "split" transactions
-- add/delete child transactions
-- enable/disable editing of parent transactions

### Todo:
- delete multiple transactions
- add categories to multiple transactions
- auto-category rule editor
-- apply new rules to existing transactions
- parse transactions from another institution.


