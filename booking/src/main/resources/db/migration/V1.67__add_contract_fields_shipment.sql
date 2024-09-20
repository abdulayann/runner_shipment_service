ALTER TABLE IF EXISTS shipment_details
    ADD COLUMN IF NOT EXISTS customer_category varchar(64),
    ADD COLUMN IF NOT EXISTS contract_id varchar(64),
    ADD COLUMN IF NOT EXISTS contract_type varchar(64);