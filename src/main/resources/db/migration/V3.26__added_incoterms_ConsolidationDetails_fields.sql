ALTER TABLE IF EXISTS consolidation_details
    ADD COLUMN IF NOT EXISTS incoterms varchar(64);