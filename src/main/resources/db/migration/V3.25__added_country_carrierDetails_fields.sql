ALTER TABLE IF EXISTS carrier_details
    ADD COLUMN IF NOT EXISTS origin_country varchar,
    ADD COLUMN IF NOT EXISTS destination_country varchar,
    ADD COLUMN IF NOT EXISTS origin_port_country varchar,
    ADD COLUMN IF NOT EXISTS destination_port_country varchar,
    ADD COLUMN IF NOT EXISTS vehicle_number varchar;