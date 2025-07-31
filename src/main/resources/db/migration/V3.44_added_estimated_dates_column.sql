ALTER TABLE IF EXISTS shipment_details ADD COLUMN IF NOT EXISTS est_brokerage_at_destination_date timestamp;
ALTER TABLE IF EXISTS shipment_details ADD COLUMN IF NOT EXISTS est_brokerage_at_origin_date timestamp;
ALTER TABLE IF EXISTS shipment_additional_details ADD COLUMN IF NOT EXISTS est_pickup_date timestamp;
ALTER TABLE IF EXISTS shipment_additional_details ADD COLUMN IF NOT EXISTS est_cargo_delivered_date timestamp;
ALTER TABLE IF EXISTS customer_booking ADD COLUMN IF NOT EXISTS est_pickup_at_origin_date timestamp;
ALTER TABLE IF EXISTS customer_booking ADD COLUMN IF NOT EXISTS est_delivery_at_destination_date timestamp;
ALTER TABLE IF EXISTS customer_booking ADD COLUMN IF NOT EXISTS est_brokerage_at_origin_date timestamp;
ALTER TABLE IF EXISTS customer_booking ADD COLUMN IF NOT EXISTS est_brokerage_at_destination_date timestamp;
