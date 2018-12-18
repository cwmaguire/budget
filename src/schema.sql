create table transaction(
  id numeric not null,
  acct_type varchar,
  acct_num  varchar,
  date date not null,
  posted date,
  cheq_num  varchar,
  desc_1    varchar not null,
  desc_2    varchar,
  cad float,
  usd float,
  parent numeric references transact(id),
  child_number,
  "note" varchar);

create table transaction_child_number(
  tx_id numeric references transaction(id),
  child_number int);

create unique index on transaction(id);

create table category(
  id serial primary key,
  name varchar unique not null);

create table transaction_category(
  id serial primary key,
  tx_id numeric references transaction(id),
  cat_id int references category(id));

create unique index on transaction_category(tx_id, cat_id);

create table category_rule(
  id serial primary key,
  match varchar,
  cat_id int references category(id))
);

create table budget_period(
  id serial primary key,
  period varchar
);

create table category_budget(
  id serial primary key,
  cat_id int references category(id),
  budget numeric,
  period_id int references budget_period(id),
  "start" timestamp without time zone,
  "end" timestamp without time zone
);
