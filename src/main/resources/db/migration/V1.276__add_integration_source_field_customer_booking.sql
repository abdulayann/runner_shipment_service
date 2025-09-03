ALTER TABLE IF EXISTS customer_booking
ADD COLUMN IF NOT EXISTS integration_source varchar(50);