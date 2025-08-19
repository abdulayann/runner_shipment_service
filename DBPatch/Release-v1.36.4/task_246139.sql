update consolidation_details
set receiving_branch=551
where id in (
    select id from consolidation_details
    where consolidation_number = 'BOMS25071256' and source_guid  is null and tenant_id =536
);