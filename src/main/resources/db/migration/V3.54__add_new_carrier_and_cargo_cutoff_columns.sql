ALTER TABLE IF EXISTS shipment_details
ADD COLUMN IF NOT EXISTS carrier_doc_cut_off timestamp,
ADD COLUMN IF NOT EXISTS carrier_receipt_cut_off timestamp;

ALTER TABLE IF EXISTS customer_booking
ADD COLUMN IF NOT EXISTS carrier_doc_cut_off timestamp,
ADD COLUMN IF NOT EXISTS carrier_receipt_cut_off timestamp;

ALTER TABLE IF EXISTS consolidation_details
ADD COLUMN IF NOT EXISTS carrier_doc_cut_off timestamp,
ADD COLUMN IF NOT EXISTS carrier_receipt_cut_off timestamp;