ALTER TABLE IF EXISTS customer_booking
    ADD COLUMN IF NOT EXISTS payment_terms varchar(50);