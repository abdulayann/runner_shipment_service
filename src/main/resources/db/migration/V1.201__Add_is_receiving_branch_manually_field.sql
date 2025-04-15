ALTER TABLE IF EXISTS consolidation_details
    ADD COLUMN IF NOT EXISTS is_receiving_branch_manually BOOLEAN;

ALTER TABLE IF EXISTS shipment_details
    ADD COLUMN IF NOT EXISTS is_receiving_branch_manually BOOLEAN;