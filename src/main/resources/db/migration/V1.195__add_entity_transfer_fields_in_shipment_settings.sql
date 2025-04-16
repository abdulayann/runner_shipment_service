ALTER TABLE IF EXISTS shipment_setting
   ADD COLUMN IF NOT EXISTS is_entity_transfer_prerequisite_enabled_date timestamp DEFAULT null,
   ADD COLUMN IF NOT EXISTS is_entity_transfer_prerequisite_enabled boolean DEFAULT false;