ALTER TABLE IF EXISTS containers
    ALTER COLUMN container_stuffing_location type VARCHAR(128);

ALTER TABLE IF EXISTS consolidation_details
    ADD COLUMN mode_of_booking type VARCHAR(64);