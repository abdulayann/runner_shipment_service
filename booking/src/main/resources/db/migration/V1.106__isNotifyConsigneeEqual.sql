ALTER TABLE IF EXISTS customer_booking
    ADD COLUMN IF NOT EXISTS is_notify_consignee_equal boolean DEFAULT false;