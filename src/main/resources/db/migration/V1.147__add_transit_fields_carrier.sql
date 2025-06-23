ALTER TABLE IF EXISTS carrier_details
    ADD COLUMN IF NOT EXISTS min_transit_hours VARCHAR(15),
    ADD COLUMN IF NOT EXISTS max_transit_hours VARCHAR(15),
    ADD COLUMN IF NOT EXISTS carrier_added_from_npm BOOLEAN DEFAULT FALSE;