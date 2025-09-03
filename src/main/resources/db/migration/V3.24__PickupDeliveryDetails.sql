ALTER TABLE IF EXISTS pickup_delivery_details
    ADD COLUMN IF NOT EXISTS ti_reference_number VARCHAR(100);