ALTER TABLE IF EXISTS shipment_setting
   ADD COLUMN IF NOT EXISTS is_network_transfer_entity_enabled boolean DEFAULT false;