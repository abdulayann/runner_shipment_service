ALTER TABLE IF EXISTS shipment_details
  ADD COLUMN IF NOT EXISTS is_migrated_to_v3 boolean DEFAULT false;

ALTER TABLE IF EXISTS consolidation_details
    ADD COLUMN IF NOT EXISTS is_migrated_to_v3 boolean DEFAULT false;


ALTER TABLE IF EXISTS notes
    ADD COLUMN IF NOT EXISTS is_read_only boolean DEFAULT false;
