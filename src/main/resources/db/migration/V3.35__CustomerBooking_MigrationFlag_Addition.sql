ALTER TABLE IF EXISTS customer_booking
    ADD COLUMN IF NOT EXISTS is_migrated_to_v3 boolean DEFAULT false;