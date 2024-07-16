ALTER TABLE IF EXISTS shipment_setting
    ADD COLUMN IF NOT EXISTS enable_lcl_consolidation boolean DEFAULT false;