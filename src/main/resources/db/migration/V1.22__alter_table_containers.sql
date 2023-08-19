alter table if exists containers
add column if not exists booking_id bigint;

alter table if exists packing
add column if not exists booking_id bigint;

alter table if exists routings
add column if not exists booking_id bigint;