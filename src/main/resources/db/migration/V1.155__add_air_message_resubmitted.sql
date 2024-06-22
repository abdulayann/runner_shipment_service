ALTER TABLE IF EXISTS shipment_db_2.public.awb
    ADD COLUMN IF NOT EXISTS air_message_resubmitted boolean;