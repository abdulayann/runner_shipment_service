ALTER TABLE IF EXISTS shipment_setting
   ADD COLUMN IF NOT EXISTS hide_manifest boolean DEFAULT true;