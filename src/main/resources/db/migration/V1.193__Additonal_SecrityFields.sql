ALTER TABLE IF EXISTS shipment_additional_details
    ADD COLUMN IF NOT EXISTS security_status_received_from VARCHAR,
    ADD COLUMN IF NOT EXISTS additional_security_information VARCHAR,
    ADD COLUMN IF NOT EXISTS regulated_entity_category VARCHAR;

ALTER TABLE IF EXISTS consolidation_details
    ADD COLUMN IF NOT EXISTS additional_security_information VARCHAR;

