ALTER TABLE IF EXISTS shipment_setting
      ADD COLUMN IF NOT EXISTS transport_order_road varchar(255);