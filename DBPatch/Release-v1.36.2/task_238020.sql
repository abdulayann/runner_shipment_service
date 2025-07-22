update consolidation_details
set receiving_branch=582
where id in(
select id from consolidation_details
where consolidation_number in('HOUS25050767','HOUS25060773') and source_guid  is null and tenant_id =470);