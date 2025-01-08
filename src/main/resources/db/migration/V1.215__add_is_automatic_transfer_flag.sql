ALTER TABLE IF EXISTS shipment_setting
    ADD COLUMN IF NOT EXISTS is_automatic_transfer_enabled BOOLEAN DEFAULT false;