ALTER TABLE IF EXISTS routings
    ADD COLUMN IF NOT EXISTS truck_reference_number VARCHAR;