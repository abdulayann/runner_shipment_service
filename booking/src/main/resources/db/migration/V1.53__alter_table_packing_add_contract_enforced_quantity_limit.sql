ALTER TABLE IF EXISTS packing
    ADD COLUMN IF NOT EXISTS contract_enforced_quantity_limit BIGINT;