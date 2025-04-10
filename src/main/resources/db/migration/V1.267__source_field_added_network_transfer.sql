ALTER TABLE IF EXISTS network_transfer
    ADD COLUMN IF NOT EXISTS source varchar;

ALTER TABLE IF EXISTS shipment_setting
    ADD COLUMN IF NOT EXISTS is_external_file_transfer_enabled BOOLEAN DEFAULT FALSE;
