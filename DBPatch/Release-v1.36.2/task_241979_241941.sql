update
	public.consolidation_details
set
	receiving_branch = 474,
	updated_at = now()
where
	consolidation_number = 'BOMS25071084'
	and tenant_id = 536;


DELETE FROM triangulation_partner_consolidation WHERE consolidation_id = 50401;
