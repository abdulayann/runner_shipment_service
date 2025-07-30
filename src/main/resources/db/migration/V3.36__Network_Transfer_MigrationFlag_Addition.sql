ALTER TABLE IF EXISTS network_transfer
    ADD COLUMN IF NOT EXISTS migration_status VARCHAR(20) ;