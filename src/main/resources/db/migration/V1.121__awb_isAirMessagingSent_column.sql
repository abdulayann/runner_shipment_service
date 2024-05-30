ALTER TABLE IF EXISTS awb
      ADD COLUMN IF NOT EXISTS is_air_messaging_sent boolean DEFAULT false;