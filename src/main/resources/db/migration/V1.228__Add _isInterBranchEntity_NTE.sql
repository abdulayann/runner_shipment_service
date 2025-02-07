ALTER TABLE IF EXISTS network_transfer
    ADD COLUMN IF NOT EXISTS is_inter_branch_entity BOOLEAN DEFAULT false;