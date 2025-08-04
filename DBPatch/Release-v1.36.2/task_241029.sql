update
	public.consolidation_details
set
	receiving_branch = 685,
	updated_at = now()
where
	consolidation_number in ('NJIS25062649','NJIS25062651')
	and tenant_id = 576;