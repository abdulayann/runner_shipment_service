ALTER TABLE IF EXISTS shipment_setting
    ADD COLUMN IF NOT EXISTS restrict_bl_edit BOOLEAN default false,
    ADD COLUMN IF NOT EXISTS restrict_awb_edit BOOLEAN default false,
    ADD COLUMN IF NOT EXISTS auto_update_shipment_awb BOOLEAN default false,
    ADD COLUMN IF NOT EXISTS auto_update_shipment_bl BOOLEAN default false;