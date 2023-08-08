ALTER TABLE IF EXISTS booking_carriage
    DROP COLUMN booking_id,
    DROP COLUMN vessel_id,
    DROP COLUMN pol_id,
    DROP COLUMN pod_id,
    ADD COLUMN port_of_loading VARCHAR,
    ADD COLUMN port_of_discharge VARCHAR;