ALTER TABLE IF EXISTS shipment_details
    ADD COLUMN goods_value decimal,
    ADD COLUMN goods_value_currency varchar(5),
    ADD COLUMN insurance_value decimal,
    ADD COLUMN insurance_value_currency varchar(5),
    ADD COLUMN shipment_created_on TIMESTAMP;
ALTER TABLE IF EXISTS shipment_additional_details
    ADD COLUMN ownership_org bigint;