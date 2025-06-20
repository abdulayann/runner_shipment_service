update network_transfer  set entity_type='SHIPMENT' where entity_type='[Shipments]' ;
update network_transfer  set entity_type='CONSOLIDATION' where entity_type='[Consolidations]' ;

UPDATE network_transfer nt SET entity_number = sd.shipment_id, transport_mode=sd.transport_mode, source_branch_id=sd.tenant_id, job_type = entity_payload->'direction'  FROM shipment_details sd WHERE nt.entity_id = sd.id AND nt.entity_type = 'SHIPMENT' AND nt.status = 'TRANSFERRED' AND nt.entity_payload is not null;
UPDATE network_transfer nt SET entity_number = sd.consolidation_number, transport_mode=sd.transport_mode, source_branch_id=sd.tenant_id, job_type = entity_payload->'shipmentType'  FROM consolidation_details sd WHERE nt.entity_id = sd.id AND nt.entity_type = 'CONSOLIDATION' AND nt.status = 'TRANSFERRED' AND nt.entity_payload is not null;

update network_transfer set job_type ='IMP' where job_type ='"IMP"';
update network_transfer set job_type ='EXP' where job_type ='"EXP"';
update network_transfer set job_type ='CTS' where job_type ='"CTS"';
