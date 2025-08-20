ALTER TABLE IF EXISTS shipment_details
    ADD COLUMN IF NOT EXISTS destination_parent_contract_id VARCHAR;