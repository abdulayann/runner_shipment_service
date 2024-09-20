
alter table if exists packing
    add column if not exists handling_info varchar(255);

alter table if exists shipment_additional_details
    add column if not exists custom_decl_type varchar(255);