ALTER TABLE IF EXISTS shipment_details
    ADD COLUMN IF NOT EXISTS destination_sales_branch varchar,
    ADD COLUMN IF NOT EXISTS destination_primary_sales_agent_email varchar,
    ADD COLUMN IF NOT EXISTS destination_secondary_sales_agent_email varchar,
    ADD COLUMN IF NOT EXISTS destination_current_party_for_quote varchar,
    ADD COLUMN IF NOT EXISTS destination_contract_id varchar,
    ADD COLUMN IF NOT EXISTS destination_contract_type varchar;