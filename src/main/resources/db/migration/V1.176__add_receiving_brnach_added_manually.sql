ALTER TABLE IF EXISTS shipment_details
    ADD COLUMN IF NOT EXISTS is_receiving_branch_added BOOLEAN DEFAULT false;