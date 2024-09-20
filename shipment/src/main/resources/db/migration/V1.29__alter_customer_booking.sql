ALTER TABLE IF EXISTS customer_booking
    DROP COLUMN contract_id,
    ADD COLUMN contract_id VARCHAR(255);
