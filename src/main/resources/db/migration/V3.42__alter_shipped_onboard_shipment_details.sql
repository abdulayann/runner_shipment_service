ALTER TABLE shipment_additional_details
ADD COLUMN IF NOT EXISTS shipped_onboard TIMESTAMP;