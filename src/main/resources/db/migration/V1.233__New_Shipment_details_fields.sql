ALTER TABLE IF EXISTS shipment_details
    ALTER COLUMN is_co_load_enabled SET DEFAULT FALSE,
    ADD COLUMN IF NOT EXISTS co_load_carrier_name VARCHAR,
    ADD COLUMN IF NOT EXISTS co_load_bl_number VARCHAR,
    ADD COLUMN IF NOT EXISTS issuing_carrier_name VARCHAR,
    ADD COLUMN IF NOT EXISTS ocean_bl_number VARCHAR;