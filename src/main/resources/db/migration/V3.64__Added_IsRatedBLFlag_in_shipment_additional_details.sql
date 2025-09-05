ALTER TABLE shipment_additional_details
    ADD COLUMN  IF NOT EXISTS is_rated_bl BOOLEAN DEFAULT FALSE;