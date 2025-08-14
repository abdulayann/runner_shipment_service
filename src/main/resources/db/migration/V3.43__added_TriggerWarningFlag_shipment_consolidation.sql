ALTER TABLE IF EXISTS shipment_details
ADD COLUMN IF NOT EXISTS trigger_migration_warning BOOLEAN default FALSE;

ALTER TABLE IF EXISTS consolidation_details
ADD COLUMN IF NOT EXISTS trigger_migration_warning BOOLEAN default FALSE;