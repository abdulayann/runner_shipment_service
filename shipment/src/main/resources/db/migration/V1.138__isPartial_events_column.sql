ALTER TABLE IF EXISTS events
    DROP COLUMN IF EXISTS partial,
    ADD COLUMN IF NOT EXISTS is_partial boolean DEFAULT false
