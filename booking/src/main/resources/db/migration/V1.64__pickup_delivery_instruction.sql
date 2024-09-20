ALTER TABLE pickup_delivery_details
    ADD COLUMN IF NOT EXISTS pickup_delivery_instruction VARCHAR(50);