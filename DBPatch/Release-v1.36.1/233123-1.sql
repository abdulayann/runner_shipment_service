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
    entity_guid,
    created_entity_id
)
SELECT
    now() AT TIME ZONE 'UTC' AS created_at,
    c.created_by,
    now() AT TIME ZONE 'UTC' AS updated_at,
    c.updated_by,
    c.receiving_branch,
    'CONSOLIDATION' AS entity_type,
    c.consolidation_number,
    c.id,
    c.transport_mode,
    c.tenant_id,
    'ACCEPTED' AS status,
    'IMP' AS direction,
    c.guid,
    cd2.id -- <== this is the child consolidation that references c.guid
FROM consolidation_details c
LEFT JOIN network_transfer nt
    ON nt.entity_id = c.id  AND nt.entity_guid = c.guid
LEFT JOIN consolidation_details cd2
    ON cd2.source_guid = c.guid AND cd2.tenant_id = c.receiving_branch and cd2.source_tenant_id =c.tenant_id
WHERE c.receiving_branch IS NOT NULL
  AND c.receiving_branch != c.tenant_id
  AND cd2.id IS NOT NULL
  AND nt.entity_id IS NULL
  and c.shipment_type = 'EXP'
  and c.source_guid IS NULL
  AND c.id not in(2634, 5498, 5883, 8484, 40697);

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
    entity_guid,
    created_entity_id
)
SELECT
    now() AT TIME ZONE 'UTC' AS created_at,
    c.created_by,
    now() AT TIME ZONE 'UTC' AS updated_at,
    c.updated_by,
    tpc.partner_id AS triangulation_branch,
    'CONSOLIDATION' AS entity_type,
    c.consolidation_number,
    c.id AS parent_id,
    c.transport_mode,
    c.tenant_id,
    'ACCEPTED' AS status,
    'CTS' AS direction,
    c.guid,
    cd2.id AS child_id
FROM consolidation_details c
JOIN triangulation_partner_consolidation tpc
    ON tpc.consolidation_id = c.id
LEFT JOIN consolidation_details cd2
    ON cd2.source_guid = c.guid
    AND cd2.source_tenant_id = c.tenant_id
    AND cd2.tenant_id = tpc.partner_id
LEFT JOIN network_transfer nt
    ON nt.entity_id = c.id AND nt.entity_guid = c.guid And nt.job_type ='CTS'
WHERE cd2.id IS NOT NULL
    and c.shipment_type = 'EXP'
    and c.source_guid IS NULL
  AND nt.entity_id IS NULL;