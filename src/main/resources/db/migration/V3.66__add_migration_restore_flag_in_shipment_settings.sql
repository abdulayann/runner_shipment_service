ALTER TABLE IF EXISTS shipment_setting
    ADD COLUMN IF NOT EXISTS is_migration_running BOOLEAN DEFAULT false,
    ADD COLUMN IF NOT EXISTS is_restore_running BOOLEAN DEFAULT false;