create table transaction(
  id numeric,
  acct_type varchar,
  acct_num  varchar,
  date date,
  posted date,
  cheq_num  varchar,
  desc_1    varchar,
  desc_2    varchar,
  cad float,
  usd float);

create unique index on transaction(id);

create table category(
  id serial primary key,
  name varchar unique not null);

create table transaction_category(
  id serial primary key,
  tx_id numeric references transaction(id),
  cat_id int references category(id));

create table category_rule(
  id serial primary key,
  match varchar,
  cat_id int references category(id));
