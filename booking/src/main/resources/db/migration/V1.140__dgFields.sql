ALTER TABLE IF EXISTS shipment_additional_details
    ADD COLUMN IF NOT EXISTS emergency_contact_number VARCHAR(31),
    ADD COLUMN IF NOT EXISTS emergency_contact_number_code VARCHAR(31);


ALTER TABLE IF EXISTS packing
    ADD COLUMN IF NOT EXISTS un_number_air VARCHAR(31),
    ADD COLUMN IF NOT EXISTS dg_class_air VARCHAR(31),
    ADD COLUMN IF NOT EXISTS dg_class_air_description VARCHAR(255);