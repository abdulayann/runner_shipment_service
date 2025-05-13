ALTER TABLE IF EXISTS customer_booking
    ADD COLUMN IF NOT EXISTS teu_count decimal,
    ADD COLUMN IF NOT EXISTS containers BIGINT,
    ADD COLUMN IF NOT EXISTS package_type varchar,
    ADD COLUMN IF NOT EXISTS packages BIGINT,
    ADD COLUMN IF NOT EXISTS description varchar,
    ADD COLUMN IF NOT EXISTS marks_n_numbers varchar,
    ADD COLUMN IF NOT EXISTS additional_terms varchar;
