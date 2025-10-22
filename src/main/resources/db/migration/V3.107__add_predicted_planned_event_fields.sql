ALTER TABLE IF EXISTS events
    ADD COLUMN IF NOT EXISTS planned_date timestamp;

ALTER TABLE IF EXISTS events
    ADD COLUMN IF NOT EXISTS predicted_date timestamp;