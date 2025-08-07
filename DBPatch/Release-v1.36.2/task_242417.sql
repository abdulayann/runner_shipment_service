UPDATE public.consolidation_details
SET receiving_branch = null
WHERE id = (select id from consolidation_details where consolidation_number = 'QINCSS25071811' and tenant_id = 710);