ALTER TABLE IF EXISTS consolidation_details
    ADD COLUMN IF NOT EXISTS is_transferred_to_receiving_branch BOOLEAN;

ALTER TABLE IF EXISTS shipment_details
    ADD COLUMN IF NOT EXISTS is_transferred_to_receiving_branch BOOLEAN;