Update shipment_details
    SET client_id = 126165
where shipment_id IN ('MILS24110421','HANS24120299','MILS25010983','MILS25011045','MILS25021266','MILS25021366')
and tenant_id = 459
and client_id = 135677;

Update shipment_details
    SET consignee_id = 126165
where shipment_id IN ('MILS24110421','HANS24120299','MILS25010983','MILS25011045','MILS25021266','MILS25021366')
and tenant_id = 459
and consignee_id = 135677;


Update shipment_additional_details
    SET notify_party_id = 126165
where shipment_id in
    (Select id from shipment_details
        where shipment_id in ('MILS24110421','HANS24120299','MILS25010983','MILS25011045','MILS25021266','MILS25021366')
    and tenant_id = 459)
and notify_party_id = 135677;