ALTER TABLE IF EXISTS shipment_additional_details
    ADD COLUMN IF NOT EXISTS summary varchar(2048),
    ADD COLUMN IF NOT EXISTS is_summary_updated boolean DEFAULT false;