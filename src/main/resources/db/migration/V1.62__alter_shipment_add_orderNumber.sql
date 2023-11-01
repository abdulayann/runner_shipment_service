ALTER TABLE shipment_details
    ADD COLUMN IF NOT EXISTS order_number BIGINT,
    ADD COLUMN IF NOT EXISTS order_management_id VARCHAR(255);