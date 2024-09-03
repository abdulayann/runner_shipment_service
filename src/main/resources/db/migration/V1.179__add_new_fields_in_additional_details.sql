alter table if exists shipment_additional_details
    add column if not exists pickup_date timestamp default null,
    add column if not exists cargo_delivered_date timestamp default null,
    add column if not exists custom_release_date timestamp default null,
    add column if not exists doc_turned_over_to_customer bool default null,
    add column if not exists proof_of_delivery_date timestamp default null,
    add column if not exists warehouse_cargo_arrival_date timestamp default null,
    add column if not exists pickup_by_consignee_completed bool default null,
    add column if not exists empty_container_returned bool default null
;

alter table if exists events
    add column if not exists container_number varchar,
    add column if not exists location_role varchar;
