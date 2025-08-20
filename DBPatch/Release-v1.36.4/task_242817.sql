update consolidation_details
set receiving_branch=459
where id in(
select id from consolidation_details
where consolidation_number ='DUBA25070486' and source_guid  is null and tenant_id=697);


update consolidation_details
set receiving_branch=551
where id in(
select id from consolidation_details
where consolidation_number ='BOMS25070976' and source_guid  is null and tenant_id=536);