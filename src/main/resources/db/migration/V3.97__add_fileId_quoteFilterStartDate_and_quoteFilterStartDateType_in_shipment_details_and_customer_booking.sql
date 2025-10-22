ALTER TABLE shipment_details
    ADD COLUMN IF NOT EXISTS quote_filter_start_date TIMESTAMP,
    ADD COLUMN IF NOT EXISTS quote_filter_start_date_type VARCHAR(50);

ALTER TABLE customer_booking
    ADD COLUMN IF NOT EXISTS quote_filter_start_date TIMESTAMP,
    ADD COLUMN IF NOT EXISTS quote_filter_start_date_type VARCHAR(50);