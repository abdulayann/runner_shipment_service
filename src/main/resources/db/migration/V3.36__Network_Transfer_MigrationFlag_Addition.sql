ALTER TABLE IF EXISTS network_transfer
     ADD COLUMN IF NOT EXISTS is_migrated_to_v3 boolean DEFAULT false;