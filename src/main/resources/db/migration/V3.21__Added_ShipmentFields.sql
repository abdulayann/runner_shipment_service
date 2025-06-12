ALTER TABLE IF EXISTS shipment_details
ADD COLUMN IF NOT EXISTS transport_info_status VARCHAR(5) DEFAULT 'Yes';