ALTER TABLE IF EXISTS shipment_setting
    ADD COLUMN IF NOT EXISTS is_awb_revamp_enabled BOOLEAN DEFAULT false;