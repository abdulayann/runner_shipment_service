update
	carrier_details
set
	destination_port = 'Charleston, US'
where
	id = (
	select
		carrier_detail_id
	from
		shipment_details
	where
		shipment_id = 'LCSS24050194'
		and tenant_id = 456);