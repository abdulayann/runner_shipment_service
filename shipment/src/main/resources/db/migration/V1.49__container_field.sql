ALTER TABLE IF EXISTS containers
    ADD COLUMN IF NOT EXISTS contract_enforced_quantity_limit BIGINT;