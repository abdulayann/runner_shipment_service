ALTER TABLE IF EXISTS ti_truck_driver_details
    ADD COLUMN IF NOT EXISTS driver_id VARCHAR(50);