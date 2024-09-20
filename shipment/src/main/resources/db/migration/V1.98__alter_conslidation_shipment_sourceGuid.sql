ALTER TABLE IF EXISTS consolidation_details
    ADD COLUMN IF NOT EXISTS source_guid uuid;
ALTER TABLE IF EXISTS shipment_details
    ADD COLUMN IF NOT EXISTS source_guid uuid;