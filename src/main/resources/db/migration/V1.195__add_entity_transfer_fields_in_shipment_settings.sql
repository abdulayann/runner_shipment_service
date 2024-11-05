ALTER TABLE IF EXISTS shipment_setting
   ADD COLUMN IF NOT EXISTS entity_transfer_enabled_date timestamp DEFAULT null,
   ADD COLUMN IF NOT EXISTS entity_transfer boolean DEFAULT false;