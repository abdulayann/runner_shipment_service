 ALTER TABLE IF EXISTS shipment_setting
  ALTER COLUMN is_ra_enabled SET DEFAULT true,
  ALTER COLUMN is_kc_enabled SET DEFAULT true;