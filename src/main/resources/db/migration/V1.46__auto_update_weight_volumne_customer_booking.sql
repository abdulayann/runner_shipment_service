alter table customer_booking
    add column if not exists auto_update_weight_volume BOOLEAN DEFAULT FALSE;