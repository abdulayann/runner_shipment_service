ALTER TABLE IF EXISTS customer_booking
    ADD COLUMN IF NOT EXISTS source_guid uuid;