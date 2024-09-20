ALTER TABLE IF EXISTS consolidation_details
    ADD COLUMN IF NOT EXISTS emergency_contact_number VARCHAR(31),
    ADD COLUMN IF NOT EXISTS emergency_contact_number_code VARCHAR(31);