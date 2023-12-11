ALTER TABLE IF EXISTS containers
    ALTER COLUMN container_stuffing_location TYPE VARCHAR(128);

ALTER TABLE IF EXISTS consolidation_details
    ADD COLUMN IF NOT EXISTS mode_of_booking VARCHAR(64);