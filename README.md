# budget
Code to import banking spreadsheets and categorize transactions

I need to check my budget everyday but Mint isn't cutting it. I want to
be able to download my transactions, import them and the categorize them
based on patterns that I input. The transactions will go into a database
table so I can query for transactions by category and date. The category
will be handled by a separate query so that a single transaction can be
in multiple categories.

So far I can parse CSVs from one bank, fetch transactions and draw them
in a table, add categories to transactions and delete them.

I still need to split transactions, add auto-category rules, apply new
rules to existing transactions and parse transactions from another
institution.
