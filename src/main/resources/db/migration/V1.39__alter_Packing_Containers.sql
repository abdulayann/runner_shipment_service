ALTER TABLE IF EXISTS containers
    ADD COLUMN commodity_group VARCHAR(255);

ALTER TABLE IF EXISTS packing
    ADD COLUMN commodity_group VARCHAR(255);
