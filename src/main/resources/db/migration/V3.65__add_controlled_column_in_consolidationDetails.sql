ALTER TABLE IF EXISTS consolidation_details
    ADD COLUMN IF NOT EXISTS controlled BOOLEAN,
    ADD COLUMN IF NOT EXISTS controlled_reference_number varchar(64);