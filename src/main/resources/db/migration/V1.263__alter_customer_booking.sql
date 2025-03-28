ALTER TABLE IF EXISTS booking_charges
    ADD COLUMN IF NOT EXISTS internal_remarks VARCHAR(255);

ALTER TABLE IF EXISTS booking_charges
    ADD COLUMN IF NOT EXISTS external_remarks VARCHAR(255);
