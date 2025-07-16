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
    shp.created_by,
    now() AT TIME ZONE 'UTC' AS updated_at,
    shp.updated_by,
    shp.receiving_branch,
    'SHIPMENT' AS entity_type,
    shp.shipment_id,
    shp.id,
    shp.transport_mode,
    shp.tenant_id,
    'ACCEPTED',
    'IMP',
    shp.guid,
    shp2.id -- <== this is the child consolidation that references c.guid
FROM shipment_details shp
LEFT JOIN network_transfer nt
    ON nt.entity_id = shp.id  AND nt.entity_guid = shp.guid
LEFT JOIN shipment_details shp2
    ON shp2.source_guid = shp.guid AND shp2.tenant_id = shp.receiving_branch and shp2.source_tenant_id =shp.tenant_id
WHERE shp.receiving_branch IS NOT NULL
  AND shp.receiving_branch != shp.tenant_id
  AND shp2.id IS NOT NULL
  AND nt.entity_id IS NULL
  and shp.job_type = 'DRT'
  and shp.transport_mode = 'AIR'
  and shp.direction = 'EXP'
  and shp.id!=8768;



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
    'SHIPMENT' AS entity_type,
    c.shipment_id,
    c.id AS parent_id,
    c.transport_mode,
    c.tenant_id,
    'ACCEPTED',
    'CTS',
    c.guid,
    cd2.id
FROM shipment_details c
JOIN triangulation_partner_shipment tpc
    ON tpc.shipment_id = c.id
LEFT JOIN shipment_details cd2
    ON cd2.source_guid = c.guid
    AND cd2.source_tenant_id = c.tenant_id
    AND cd2.tenant_id = tpc.partner_id
LEFT JOIN network_transfer nt
    ON nt.entity_id = c.id AND nt.entity_guid = c.guid AND nt.job_type ='CTS'
WHERE cd2.id IS NOT NULL
  AND nt.entity_id IS NULL
  and c.job_type = 'DRT'
    and c.transport_mode = 'AIR'
    and c.direction = 'EXP';

