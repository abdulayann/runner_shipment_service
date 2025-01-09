ALTER TABLE IF EXISTS shipment_setting
   ADD COLUMN IF NOT EXISTS always_utilization boolean DEFAULT false,
   ADD COLUMN IF NOT EXISTS utilization_for_container_quoted boolean DEFAULT false,
   ADD COLUMN IF NOT EXISTS no_utilization boolean DEFAULT true;