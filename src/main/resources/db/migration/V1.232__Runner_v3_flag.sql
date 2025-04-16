ALTER TABLE IF EXISTS shipment_setting
  ADD COLUMN IF NOT EXISTS is_runner_v3_enabled boolean DEFAULT false;