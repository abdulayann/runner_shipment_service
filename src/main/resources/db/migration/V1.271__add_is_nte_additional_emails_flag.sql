ALTER TABLE IF EXISTS shipment_setting
    ADD COLUMN IF NOT EXISTS is_nte_additional_emails_enabled BOOLEAN DEFAULT false;