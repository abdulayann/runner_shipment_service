ALTER TABLE IF EXISTS ti_packages
    ADD COLUMN IF NOT EXISTS length_unit VARCHAR(10),
    ADD COLUMN IF NOT EXISTS width_unit VARCHAR(10),
    ADD COLUMN IF NOT EXISTS height_unit VARCHAR(10),
    ADD COLUMN IF NOT EXISTS "length" numeric(5,2),
    ADD COLUMN IF NOT EXISTS width numeric(5,2),
    ADD COLUMN IF NOT EXISTS height numeric(5,2);
