ALTER TABLE IF EXISTS shipment_details
    ADD COLUMN IF NOT EXISTS cloned_guid uuid;