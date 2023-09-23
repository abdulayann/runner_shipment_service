ALTER TABLE IF EXISTS containers
    ADD COLUMN is_contract_enforced BOOLEAN default false;
