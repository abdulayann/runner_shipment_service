Delete from network_transfer where status='SCHEDULED';

UPDATE network_transfer nt
SET entity_guid = (entity_payload->>'guid')::uuid
WHERE nt.status = 'TRANSFERRED'
  AND nt.entity_payload IS NOT NULL;



INSERT INTO network_transfer (
    created_at,
    created_by ,
    updated_at,
    updated_by,
    tenant_id,
    entity_type,
    entity_number,
    entity_id,
    transport_mode,
    source_branch_id,
    status,
    job_type,
    entity_guid
)
SELECT
    now() AT TIME ZONE 'UTC',
    c.created_by,
    now() AT TIME ZONE 'UTC',
    c.updated_by,
    c.receiving_branch,
    'CONSOLIDATION',
    c.consolidation_number,
    c.id,
    c.transport_mode,
    c.tenant_id,
    'SCHEDULED',
    'IMP',
    c.guid
FROM consolidation_details c
LEFT JOIN network_transfer nt
    ON nt.entity_id = c.id AND nt.entity_guid = c.guid
WHERE c.receiving_branch IS NOT NULL
  AND nt.entity_id IS null
   and c.source_guid is null
   and (c.guid not in (select cd2.source_guid from consolidation_details cd2 where cd2.source_guid = c.guid))
  and shipment_type = 'EXP'
  and c.receiving_branch!=c.tenant_id;



INSERT INTO network_transfer (
    created_at,
    created_by ,
    updated_at,
    updated_by,
    tenant_id,
    entity_type,
    entity_number,
    entity_id,
    transport_mode,
    source_branch_id,
    status,
    job_type,
    entity_guid
)
SELECT
    now() AT TIME ZONE 'UTC',
    sd.created_by,
    now() AT TIME ZONE 'UTC',
    sd.updated_by,
    sd.receiving_branch,
    'SHIPMENT',
    sd.shipment_id,
    sd.id,
    sd.transport_mode,
    sd.tenant_id,
    'SCHEDULED',
    'IMP',
    sd.guid
FROM shipment_details sd
LEFT JOIN network_transfer nt
    ON nt.entity_id = sd.id AND nt.entity_guid = sd.guid
where sd.receiving_branch is not null
AND nt.entity_id is null
and sd.source_guid is null
and (sd.guid not in (select sd2.source_guid from shipment_details sd2 where sd2.source_guid = sd.guid))
and sd.job_type = 'DRT'
and sd.transport_mode = 'AIR'
and sd.direction = 'EXP'
and sd.receiving_branch!=sd.tenant_id;

