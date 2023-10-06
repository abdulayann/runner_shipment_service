ALTER TABLE IF EXISTS shipment_details
    ADD COLUMN IF NOT EXISTS entryRefNo VARCHAR(255);

ALTER TABLE IF EXISTS carrier_details
    ADD COLUMN IF NOT EXISTS vesselBerthingDate timestamp;

ALTER TABLE IF EXISTS reference_numbers
    ADD COLUMN IF NOT EXISTS isPortalEnable BOOLEAN default false;