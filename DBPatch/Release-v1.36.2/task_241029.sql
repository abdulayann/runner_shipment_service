update
	public.consolidation_details
set
	receiving_branch = 685,
	updated_at = now()
where
	consolidation_number = 'NJIS25062649'
	and tenant_id = 576;