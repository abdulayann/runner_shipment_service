ALTER TABLE IF EXISTS routings
    ADD COLUMN IF NOT EXISTS origin_port_loc_code varchar(64),
    ADD COLUMN IF NOT EXISTS destination_port_loc_code varchar(64);