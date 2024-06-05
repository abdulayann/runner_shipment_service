alter table if exists shipment_setting
    add column if not exists booking_order varchar,
    add column if not exists booking_order_mbl varchar,
    add column if not exists booking_order_air varchar,
    add column if not exists booking_order_air_mawb varchar;