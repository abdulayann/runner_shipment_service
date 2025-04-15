ALTER TABLE customer_booking
    ADD COLUMN IF NOT EXISTS order_management_number VARCHAR,
    ADD COLUMN IF NOT EXISTS order_management_id VARCHAR(255);