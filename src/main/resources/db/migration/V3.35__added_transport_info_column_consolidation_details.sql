ALTER TABLE IF EXISTS consolidation_details
ADD COLUMN IF NOT EXISTS transport_info_status VARCHAR(5) DEFAULT 'YES';