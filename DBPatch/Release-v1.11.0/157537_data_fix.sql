update
	carrier_details
set
	destination_port = 'tyqvi1CClzB5eQfivpCD'
where
	id = (
	select
		carrier_detail_id
	from
		shipment_details
	where
		shipment_id = 'JKTS24050098'
		and tenant_id = 485);