ALTER TABLE IF EXISTS shipment_details
      ADD COLUMN IF NOT EXISTS consignee_dps_address_id BIGINT null,
      ADD COLUMN IF NOT EXISTS client_dps_address_id BIGINT null,
      ADD COLUMN IF NOT EXISTS consignor_dps_address_id BIGINT null,
      ADD COLUMN IF NOT EXISTS notify_party_dps_address_id BIGINT null;