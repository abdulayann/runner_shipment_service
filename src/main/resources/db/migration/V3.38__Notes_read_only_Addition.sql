ALTER TABLE IF EXISTS notes
    ADD COLUMN IF NOT EXISTS is_read_only boolean DEFAULT false;
