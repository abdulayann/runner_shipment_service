ALTER TABLE IF EXISTS consolidation_details
    ADD COLUMN IF NOT EXISTS booking_id VARCHAR;
ALTER TABLE IF EXISTS consolidation_details
    ADD COLUMN IF NOT EXISTS booking_status VARCHAR;
ALTER TABLE IF EXISTS consolidation_details
    ADD COLUMN IF NOT EXISTS booking_number VARCHAR;