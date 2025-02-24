ALTER TABLE IF EXISTS shipment_setting
  ADD COLUMN IF NOT EXISTS country_air_cargo_security boolean DEFAULT false;