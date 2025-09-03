ALTER TABLE IF EXISTS containers
    ADD COLUMN IF NOT EXISTS teu numeric(19,2),
    ADD COLUMN IF NOT EXISTS is_assigned boolean DEFAULT false;
