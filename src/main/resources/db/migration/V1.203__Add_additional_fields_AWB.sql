ALTER TABLE IF EXISTS awb
    ADD COLUMN IF NOT EXISTS air_messaging_additional_fields jsonb;
