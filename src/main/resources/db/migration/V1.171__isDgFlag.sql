alter table if exists customer_booking
ADD COLUMN IF NOT EXISTS is_dg BOOLEAN DEFAULT false;