ALTER TABLE IF EXISTS consolidation_details
    ADD COLUMN IF NOT EXISTS consolidation_status VARCHAR(63) DEFAULT 'Created';

ALTER TABLE IF EXISTS shipment_setting
    ADD COLUMN IF NOT EXISTS enable_consolidation_status BOOLEAN DEFAULT false;