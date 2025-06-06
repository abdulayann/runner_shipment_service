ALTER TABLE IF EXISTS carrier_details
    ADD COLUMN IF NOT EXISTS is_same_as_origin_port boolean DEFAULT false,
    ADD COLUMN IF NOT EXISTS is_same_as_destination_port boolean DEFAULT false;
