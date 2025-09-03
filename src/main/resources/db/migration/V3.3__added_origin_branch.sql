ALTER TABLE IF EXISTS shipment_details
    ADD COLUMN IF NOT EXISTS origin_branch bigint;

ALTER TABLE IF EXISTS consolidation_details
    ADD COLUMN IF NOT EXISTS origin_branch bigint;