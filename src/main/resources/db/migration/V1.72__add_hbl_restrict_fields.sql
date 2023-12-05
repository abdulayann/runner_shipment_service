ALTER TABLE IF EXISTS shipment_setting
    ADD COLUMN IF NOT EXISTS restrict_bl_release boolean,
    ADD COLUMN IF NOT EXISTS restrict_bl_approval_role INTEGER,
    ADD COLUMN IF NOT EXISTS e_manifest boolean;
