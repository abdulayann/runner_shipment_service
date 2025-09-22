ALTER TABLE IF EXISTS shipment_details
ADD COLUMN IF NOT EXISTS migration_status VARCHAR(20) default 'CREATED_IN_V2';

ALTER TABLE IF EXISTS consolidation_details
ADD COLUMN IF NOT EXISTS migration_status VARCHAR(20) default 'CREATED_IN_V2';

ALTER TABLE IF EXISTS customer_booking
ADD COLUMN IF NOT EXISTS migration_status VARCHAR(20) default 'CREATED_IN_V2';