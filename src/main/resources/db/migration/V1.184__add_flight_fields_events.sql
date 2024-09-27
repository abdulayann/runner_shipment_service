alter table if exists events
    add column if not exists flight_number varchar,
    add column if not exists flight_name varchar;