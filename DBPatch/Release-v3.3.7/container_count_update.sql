update containers
set container_count=1
where tenant_id in(536,482,571,507,506) and container_count>1 and is_deleted=false and consolidation_id is not null