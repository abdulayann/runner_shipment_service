ALTER TABLE shipment_additional_details
    ADD COLUMN IF NOT EXISTS exemption_codes VARCHAR(255),
    ADD COLUMN IF NOT EXISTS aom_free_text VARCHAR(255);