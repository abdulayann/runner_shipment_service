ALTER TABLE shipment_details
    RENAME COLUMN quote_filter_start_date TO quote_date;

ALTER TABLE shipment_details
    RENAME COLUMN quote_filter_start_date_type TO quote_date_type;

ALTER TABLE customer_booking
    RENAME COLUMN quote_filter_start_date TO quote_date;

ALTER TABLE customer_booking
    RENAME COLUMN quote_filter_start_date_type TO quote_date_type;