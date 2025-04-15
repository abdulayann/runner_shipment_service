alter table if exists shipment_setting
    add column if not exists transport_instruction_pickup_order varchar,
    add column if not exists transport_instruction_delivery_order varchar;
