ALTER TABLE IF EXISTS shipment_setting
   ADD COLUMN IF NOT EXISTS is_always_utilization boolean DEFAULT false,
   ADD COLUMN IF NOT EXISTS is_utilization_for_container_quoted boolean DEFAULT false,
   ADD COLUMN IF NOT EXISTS has_no_utilization boolean DEFAULT true;