Delete from network_transfer where entity_id=60356 and tenant_id=709;

update consolidation_details
set receiving_branch=582
where id=60356;
