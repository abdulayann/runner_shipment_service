update
	carrier_details
set
	destination_port = 'CASJB_POR',
	updated_at = current_timestamp
where
    id = (
    select
        carrier_detail_id
    from
        shipment_details
    where
        shipment_id = 'HAMS24050160'
        and tenant_id = 474);