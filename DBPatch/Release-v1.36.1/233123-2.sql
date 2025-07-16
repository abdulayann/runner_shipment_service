UPDATE consolidation_details c
SET is_transferred_to_receiving_branch = TRUE
WHERE EXISTS (
    SELECT 1
    FROM consolidation_details cd2
    JOIN network_transfer nt
        ON nt.entity_id = c.id
        AND nt.entity_guid = c.guid
        AND nt.entity_type = 'CONSOLIDATION'
        AND nt.job_type = 'IMP'
        AND nt.status = 'ACCEPTED'
    WHERE cd2.source_guid = c.guid
      AND cd2.source_tenant_id = c.tenant_id
      AND cd2.tenant_id = c.receiving_branch
      AND c.receiving_branch IS NOT NULL
      AND c.receiving_branch != c.tenant_id
) AND c.is_transferred_to_receiving_branch=false;

UPDATE triangulation_partner_consolidation tpc
SET is_accepted = TRUE
WHERE EXISTS (
    SELECT 1
    FROM consolidation_details c
    LEFT JOIN consolidation_details cd2
        ON cd2.source_guid = c.guid
        AND cd2.source_tenant_id = c.tenant_id
        AND cd2.tenant_id = tpc.partner_id
        AND nt.status = 'ACCEPTED'
    JOIN network_transfer nt
        ON nt.entity_id = c.id
        AND nt.entity_guid = c.guid
        AND nt.tenant_id = tpc.partner_id  -- extra condition
        AND nt.job_type = 'CTS'
    WHERE c.id = tpc.consolidation_id
      AND cd2.id IS NOT NULL
) AND tpc.is_accepted = false;
