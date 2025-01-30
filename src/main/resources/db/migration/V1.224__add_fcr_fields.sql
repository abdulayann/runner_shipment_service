ALTER TABLE shipment_additional_details ADD COLUMN fcr_number integer DEFAULT 0;
ALTER TABLE shipment_setting ADD COLUMN fcr_document varchar;