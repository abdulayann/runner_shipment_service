ALTER TABLE IF EXISTS shipment_setting
  ADD COLUMN IF NOT EXISTS allow_unassigned_bl_inv_generation boolean DEFAULT true;