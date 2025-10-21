ALTER TABLE consolidation_details
    ADD COLUMN IF NOT EXISTS security_status_received_from VARCHAR(100);

ALTER TABLE consolidation_details
    ADD COLUMN IF NOT EXISTS expiry_date TIMESTAMP;

ALTER TABLE consolidation_details
    ADD COLUMN IF NOT EXISTS regulated_entity_category VARCHAR(255);

