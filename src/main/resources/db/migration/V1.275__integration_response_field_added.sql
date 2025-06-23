ALTER TABLE IF EXISTS integration_responses
    ADD COLUMN IF NOT EXISTS request_body jsonb;