ALTER TABLE IF EXISTS  services DROP service_duration;

ALTER TABLE IF EXISTS services
    ALTER COLUMN srv_location TYPE varchar(255),
    ADD COLUMN service_duration time;