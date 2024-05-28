ALTER TABLE IF EXISTS awb
    ADD COLUMN IF NOT EXISTS enable_fetch_rates_warning boolean DEFAULT false;