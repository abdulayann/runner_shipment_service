Delete from network_transfer where entity_id=72385 and entity_type = 'CONSOLIDATION' and tenant_id=490;

update consolidation_details
set receiving_branch=646
where id=72385;