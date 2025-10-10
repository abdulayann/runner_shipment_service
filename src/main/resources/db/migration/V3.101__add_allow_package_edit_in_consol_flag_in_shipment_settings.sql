ALTER TABLE IF EXISTS shipment_setting
    ADD COLUMN IF NOT EXISTS is_allow_package_edit_in_console BOOLEAN DEFAULT false;