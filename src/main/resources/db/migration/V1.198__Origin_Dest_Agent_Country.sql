ALTER TABLE IF EXISTS shipment_additional_details
    ADD COLUMN IF NOT EXISTS import_broker_country VARCHAR(3),
    ADD COLUMN IF NOT EXISTS export_broker_country VARCHAR(3);

ALTER TABLE IF EXISTS consolidation_details
    ADD COLUMN IF NOT EXISTS sending_agent_country VARCHAR(3),
    ADD COLUMN IF NOT EXISTS receiving_agent_country VARCHAR(3);