ALTER TABLE IF EXISTS events
    ADD COLUMN IF NOT EXISTS event_type varchar(255);
