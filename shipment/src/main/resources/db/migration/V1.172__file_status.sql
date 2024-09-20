alter table if exists shipment_details
    ADD COLUMN IF NOT EXISTS file_status varchar(55);

