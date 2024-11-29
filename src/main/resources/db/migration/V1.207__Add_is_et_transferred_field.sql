ALTER TABLE IF EXISTS consolidation_details
    ADD COLUMN IF NOT EXISTS is_et_transferred BOOLEAN;

ALTER TABLE IF EXISTS shipment_details
    ADD COLUMN IF NOT EXISTS is_et_transferred BOOLEAN;