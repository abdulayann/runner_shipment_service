alter table if exists routings
    add column if not exists vehicle_number varchar;