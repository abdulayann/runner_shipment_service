ALTER TABLE IF EXISTS customer_booking
    ADD COLUMN IF NOT EXISTS shipment_guid VARCHAR,
    ADD COLUMN IF NOT EXISTS shipment_entity_id_v2 VARCHAR;