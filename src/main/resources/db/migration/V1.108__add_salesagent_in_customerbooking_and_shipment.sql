ALTER TABLE IF EXISTS customer_booking
    DROP COLUMN IF EXISTS sales_agent_email,
    ADD COLUMN IF NOT EXISTS primary_sales_agent_email VARCHAR,
    ADD COLUMN IF NOT EXISTS secondary_sales_agent_email VARCHAR;

ALTER TABLE IF EXISTS shipment_details
    DROP COLUMN IF EXISTS sales_agent_email,
    ADD COLUMN IF NOT EXISTS primary_sales_agent_email VARCHAR,
    ADD COLUMN IF NOT EXISTS secondary_sales_agent_email VARCHAR;