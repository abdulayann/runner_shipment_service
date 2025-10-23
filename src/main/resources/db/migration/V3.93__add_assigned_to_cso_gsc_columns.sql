ALTER TABLE IF EXISTS shipment_details
    ADD COLUMN IF NOT EXISTS assigned_to_cso INTEGER,
    ADD COLUMN IF NOT EXISTS assigned_to_gsc INTEGER;
