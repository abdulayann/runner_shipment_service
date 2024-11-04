ALTER TABLE IF EXISTS consolidation_details
    ADD COLUMN IF NOT EXISTS is_network_file BOOLEAN;

ALTER TABLE IF EXISTS shipment_details
    ADD COLUMN IF NOT EXISTS is_network_file BOOLEAN;

