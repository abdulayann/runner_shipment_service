ALTER TABLE IF EXISTS shipment_setting
      ADD COLUMN IF NOT EXISTS regulated_agent boolean DEFAULT false,
      ADD COLUMN IF NOT EXISTS ra_number VARCHAR null,
      ADD COLUMN IF NOT EXISTS ra_expiry timestamp null;

ALTER TABLE IF EXISTS consolidation_details
    ADD COLUMN IF NOT EXISTS efreight_status VARCHAR null;