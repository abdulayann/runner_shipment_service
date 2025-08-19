update consolidation_details
set receiving_branch=713
where id in (
    select id from consolidation_details
    where consolidation_number = 'BCNS25062296' and source_guid  is null and tenant_id =520
);