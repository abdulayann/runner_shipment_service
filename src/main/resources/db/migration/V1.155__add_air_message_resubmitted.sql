ALTER TABLE IF EXISTS awb
    ADD COLUMN IF NOT EXISTS air_message_resubmitted boolean;