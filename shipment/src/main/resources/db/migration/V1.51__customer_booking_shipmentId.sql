ALTER TABLE IF EXISTS customer_booking
    ADD COLUMN IF NOT EXISTS shipment_entity_id VARCHAR;