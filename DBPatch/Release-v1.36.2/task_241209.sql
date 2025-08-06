update
	mawb_stocks_link
set
	status = 'Unused',
	updated_at = NOW(),
	entity_type = null,
	entity_id = null,
	ship_cons_number = null
where
	mawb_number = '157-05955935';

update
	consolidation_details
set
	bol = '157-05955935'
where
	id = 63931;


update
	awb
set
	awb_number = '157-05955935'
where
	consolidation_id = 63931;