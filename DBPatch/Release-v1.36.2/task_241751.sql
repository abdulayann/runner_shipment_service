update
	public.consolidation_details
set
	receiving_branch = 468,
	updated_at = now()
where
	consolidation_number = 'SNZCSS25070971'
	and tenant_id = 713;