update
	public.consolidation_details
set
	receiving_branch = 590,
	updated_at = now()
where
	consolidation_number = 'QINCSS25061102'
	and tenant_id = 710;