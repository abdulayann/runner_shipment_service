ALTER TABLE IF EXISTS shipment_details
ADD COLUMN IF NOT EXISTS sync_routing_from_consolidation BOOLEAN;