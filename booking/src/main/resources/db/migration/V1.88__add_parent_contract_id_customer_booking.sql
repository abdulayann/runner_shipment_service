ALTER TABLE IF EXISTS customer_booking
    ADD COLUMN IF NOT EXISTS parent_contract_id VARCHAR;