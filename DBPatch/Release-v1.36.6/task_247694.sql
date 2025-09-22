update consolidation_details
set receiving_branch=590, updated_at=NOW()
where id in (58310, 58059);

Delete from network_transfer where tenant_id= 591 and entity_id in (58310, 58059);