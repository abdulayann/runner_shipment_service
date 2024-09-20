alter table if exists booking_charges
    drop column local_cost_currency,
    add column local_cost_currency varchar(50),
    drop column local_sell_currency,
    add column local_sell_currency varchar(50);

