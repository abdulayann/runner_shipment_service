ALTER TABLE IF EXISTS shipment_setting
      ADD COLUMN IF NOT EXISTS disable_bl_parties_name boolean DEFAULT false;