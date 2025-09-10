update consolidation_details
set receiving_branch=520, updated_at= NOW()
where id in (60073);

Delete from network_transfer where tenant_id= 484 and entity_number ='DMMA25070210';