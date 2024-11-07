ALTER TABLE IF EXISTS consolidation_details
    ADD COLUMN IF NOT EXISTS carrier_booking_id bigint;
