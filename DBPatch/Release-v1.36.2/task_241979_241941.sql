update
	public.consolidation_details
set
	receiving_branch = 474,
	updated_at = now()
where
	consolidation_number = 'BOMS25071084'
	and tenant_id = 536;


update
	public.consolidation_details
set
	receiving_branch = null,
	updated_at = now()
where
	consolidation_number = 'SNZCSS25060555'
	and tenant_id = 713;
