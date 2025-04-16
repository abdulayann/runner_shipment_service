ALTER TABLE IF EXISTS events
    ADD COLUMN IF NOT EXISTS branch_name varchar;