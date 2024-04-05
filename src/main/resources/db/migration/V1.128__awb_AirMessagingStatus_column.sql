ALTER TABLE IF EXISTS awb
      ADD COLUMN IF NOT EXISTS air_message_status VARCHAR,
      ADD COLUMN IF NOT EXISTS linked_hawb_air_message_status VARCHAR,
      DROP COLUMN IF EXISTS is_air_messaging_sent;

ALTER TABLE IF EXISTS events
    ADD COLUMN IF NOT EXISTS status VARCHAR,
    ADD COLUMN IF NOT EXISTS pieces INTEGER,
    ADD COLUMN IF NOT EXISTS total_pieces INTEGER,
    ADD COLUMN IF NOT EXISTS weight numeric(19,2),
    ADD COLUMN IF NOT EXISTS total_weight numeric(19,2),
    ADD COLUMN IF NOT EXISTS partial VARCHAR,
    ADD COLUMN IF NOT EXISTS received_date TIMESTAMP,
    ADD COLUMN IF NOT EXISTS scheduled_date TIMESTAMP;
