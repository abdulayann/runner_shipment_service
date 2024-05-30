ALTER TABLE IF EXISTS shipment_setting
    ADD COLUMN IF NOT EXISTS cancelled_bl_suffix varchar(5);