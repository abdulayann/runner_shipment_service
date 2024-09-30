ALTER TABLE shipment_additional_details
    ADD COLUMN IF NOT EXISTS agent_reference VARCHAR;