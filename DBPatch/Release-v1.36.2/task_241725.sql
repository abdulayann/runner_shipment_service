update
	public.consolidation_details
set
	receiving_branch = 713,
	updated_at = now()
where
	consolidation_number = 'BCNS25062296'
	and tenant_id = 520;