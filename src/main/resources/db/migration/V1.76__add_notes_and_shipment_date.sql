ALTER TABLE IF EXISTS notes
    ADD COLUMN label VARCHAR,
    ADD COLUMN assigned_to VARCHAR;

ALTER TABLE IF EXISTS customer_booking
    ADD COLUMN shipment_created_date timestamp;