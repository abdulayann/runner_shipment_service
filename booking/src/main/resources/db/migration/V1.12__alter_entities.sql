ALTER TABLE IF EXISTS shipment_additional_details
    ALTER COLUMN paid_place type VARCHAR,
	DROP COLUMN place_of_issue_id,
    ADD COLUMN place_of_issue VARCHAR,
    ALTER COLUMN place_of_supply type VARCHAR;

ALTER TABLE IF EXISTS shipment_details
    ADD COLUMN pickup_details_id BIGINT,
    ADD COLUMN delivery_details_id BIGINT;

ALTER TABLE IF EXISTS arrival_departure_details
    DROP COLUMN arrival_first_arrival_port_arrival_date,
    DROP COLUMN arrival_last_foreign_port_departure_date,
    DROP COLUMN departure_first_foreign_port_arrival_date,
    DROP COLUMN departure_last_foreign_port_departure_date,
    DROP COLUMN arrival_container_yard_id,
    DROP COLUMN arrival_first_arrival_port_id,
    DROP COLUMN arrival_last_foreign_port_id,
    DROP COLUMN arrival_transport_port_id,
    DROP COLUMN departure_container_yard_id,
    DROP COLUMN departure_first_foreign_port_id,
    DROP COLUMN departure_last_foreign_port_id,
    DROP COLUMN departure_transport_port_id,
    ADD COLUMN container_yard_id BIGINT,
    ADD COLUMN transport_port_id BIGINT,
    ADD COLUMN cto_id BIGINT,
    ADD COLUMN cfs_id BIGINT,
    ADD COLUMN first_foreign_port_id BIGINT,
    ADD COLUMN last_foreign_port_id BIGINT,
    ADD COLUMN first_foreign_port VARCHAR,
    ADD COLUMN last_foreign_port VARCHAR,
    ADD COLUMN type VARCHAR,
    ADD COLUMN first_foreign_port_arrival_date TIMESTAMP,
    ADD COLUMN last_foreign_port_departure_date TIMESTAMP;
