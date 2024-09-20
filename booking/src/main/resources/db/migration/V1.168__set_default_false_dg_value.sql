ALTER TABLE shipment_details
ALTER COLUMN contains_hazardous set default false;

Update shipment_details set contains_hazardous = false where contains_hazardous is null;

ALTER TABLE packing
ALTER COLUMN hazardous set default false;

Update packing set hazardous = false where hazardous is null;

ALTER TABLE consolidation_details
ALTER COLUMN hazardous set default false;

Update consolidation_details set hazardous = false where hazardous is null;