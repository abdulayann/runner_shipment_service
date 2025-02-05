ALTER TABLE IF EXISTS shipment_details
    ADD COLUMN IF NOT EXISTS is_co_load_enabled BOOLEAN DEFAULT FALSE,
    ADD COLUMN IF NOT EXISTS co_load_carrier_name VARCHAR,
    ADD COLUMN IF NOT EXISTS co_load_bl_number VARCHAR,
    ADD COLUMN IF NOT EXISTS issuing_carrier_name VARCHAR,
    ADD COLUMN IF NOT EXISTS ocean_bl_number VARCHAR;