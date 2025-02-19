ALTER TABLE IF EXISTS shipment_setting
  ADD COLUMN IF NOT EXISTS is_ra_enabled boolean DEFAULT false,
  ADD COLUMN IF NOT EXISTS is_kc_enabled boolean DEFAULT false;