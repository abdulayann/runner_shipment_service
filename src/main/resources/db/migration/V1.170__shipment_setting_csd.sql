alter table if exists shipment_setting
    add column if not exists csd varchar(64);