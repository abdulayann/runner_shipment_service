alter table if exists shipment_details
ADD COLUMN IF NOT EXISTS parent_contract_id VARCHAR;