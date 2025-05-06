ALTER TABLE IF EXISTS customer_booking
ADD COLUMN IF NOT EXISTS shipment_reference_number varchar(50);