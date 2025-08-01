update
	mawb_stocks_link
set
	status = 'Unused',
	updated_at = NOW(),
	entity_type = null,
	entity_id = null,
	ship_cons_number = null
where
	id = 8082;

update
	consolidation_details
set
	bol = '180-41370965'
where
	id = 63511;

update
	shipment_details
set
	master_bill = '180-41370965'
where
	id = 83639;

update
	awb
set
	awb_number = '180-41370965'
where
	id in (1596, 1599);