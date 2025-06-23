ALTER TABLE IF EXISTS shipment_setting
    ADD COLUMN IF NOT EXISTS enable_party_check_for_consolidation boolean DEFAULT false;