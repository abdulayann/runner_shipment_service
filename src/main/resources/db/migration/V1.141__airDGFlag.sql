ALTER TABLE IF EXISTS shipment_setting
    ADD COLUMN IF NOT EXISTS air_dg_flag boolean DEFAULT false;