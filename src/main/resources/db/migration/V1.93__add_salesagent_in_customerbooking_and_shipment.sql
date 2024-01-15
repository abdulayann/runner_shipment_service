ALTER TABLE IF EXISTS customer_booking
    ADD COLUMN IF NOT EXISTS sales_agent_email VARCHAR,
    ADD COLUMN IF NOT EXISTS sales_branch VARCHAR;

ALTER TABLE IF EXISTS shipment_details
    ADD COLUMN IF NOT EXISTS sales_agent_email VARCHAR,
    ADD COLUMN IF NOT EXISTS sales_branch VARCHAR;