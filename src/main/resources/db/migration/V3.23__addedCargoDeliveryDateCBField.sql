ALTER TABLE IF EXISTS customer_booking
    ADD COLUMN IF NOT EXISTS cargo_delivery_date timestamp;