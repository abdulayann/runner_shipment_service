ALTER TABLE IF EXISTS events
    ADD COLUMN IF NOT EXISTS direction varchar(255);
