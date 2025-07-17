ALTER TABLE IF EXISTS customer_booking
    ADD COLUMN IF NOT EXISTS co_load_carrier_name VARCHAR(64);