update
	shipment_details
set
	status = 3,
	updated_at = current_timestamp
where
	shipment_id = 'BOMA24050357'
	and tenant_id = 536;
