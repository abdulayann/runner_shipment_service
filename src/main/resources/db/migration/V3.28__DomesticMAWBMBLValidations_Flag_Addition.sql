ALTER TABLE IF EXISTS shipment_setting
  ADD COLUMN IF NOT EXISTS enable_domestic_mawb_mbl_validations boolean DEFAULT false;