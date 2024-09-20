ALTER TABLE IF EXISTS shipment_details
    ADD COLUMN IF NOT EXISTS entry_ref_no VARCHAR(255);

ALTER TABLE IF EXISTS carrier_details
    ADD COLUMN IF NOT EXISTS vessel_berthing_date timestamp;

ALTER TABLE IF EXISTS reference_numbers
    ADD COLUMN IF NOT EXISTS is_portal_enable BOOLEAN default false;