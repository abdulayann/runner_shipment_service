ALTER TABLE shipment_additional_details
    ADD COLUMN IF NOT EXISTS exemption_codes VARCHAR(255);