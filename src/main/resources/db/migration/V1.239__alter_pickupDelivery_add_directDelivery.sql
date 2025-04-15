ALTER TABLE IF EXISTS pickup_delivery_details
  ADD COLUMN IF NOT EXISTS is_direct_delivery boolean DEFAULT false;