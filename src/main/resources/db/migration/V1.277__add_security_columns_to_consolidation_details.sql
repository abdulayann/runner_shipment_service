ALTER TABLE consolidation_details
    ADD COLUMN security_status_received_from VARCHAR(100),
    ADD COLUMN expiry_date TIMESTAMP,
    ADD COLUMN regulated_entity_category VARCHAR(255);
