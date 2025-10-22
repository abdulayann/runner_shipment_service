ALTER TABLE IF EXISTS consolidation_details ADD COLUMN IF NOT EXISTS parent_guid uuid;
ALTER TABLE IF EXISTS consolidation_details ADD COLUMN IF NOT EXISTS parent_tenant_id bigint;
ALTER TABLE IF EXISTS shipment_details ADD COLUMN IF NOT EXISTS parent_guid uuid;
ALTER TABLE IF EXISTS shipment_details ADD COLUMN IF NOT EXISTS parent_tenant_id bigint;