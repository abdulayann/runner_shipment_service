-- Backfill parent_guid and parent_tenant_id in shipment_details and consolidation_details

UPDATE shipment_details child
SET
    parent_guid = parent.guid,
    parent_tenant_id = parent.tenant_id
FROM shipment_details parent
WHERE
    child.shipment_id = parent.shipment_id
    AND child.guid != child.source_guid
    AND parent.guid = parent.source_guid
    AND child.is_deleted = false
    AND parent.is_deleted = false
    AND child.shipment_id IN (
        -- only update shipment_ids that have both parent and child
        SELECT DISTINCT c.shipment_id
        FROM shipment_details c
        INNER JOIN shipment_details p
            ON c.source_guid = p.guid
        WHERE c.guid != c.source_guid
          AND c.is_deleted = false
          AND p.is_deleted = false
    );

UPDATE consolidation_details child
SET
    parent_guid = parent.guid,
    parent_tenant_id = parent.tenant_id
FROM consolidation_details parent
WHERE
    child.consolidation_number = parent.consolidation_number
    AND child.guid != child.source_guid
    AND parent.guid = parent.source_guid
    AND child.is_deleted = false
    AND parent.is_deleted = false
    AND child.consolidation_number IN (
        -- Only update consolidations that have both parent and child
        SELECT DISTINCT c.consolidation_number
        FROM consolidation_details c
        INNER JOIN consolidation_details p
            ON c.source_guid = p.guid
        WHERE c.guid != c.source_guid
            AND c.is_deleted = false
            AND p.is_deleted = false
    );

--================================================================================
-- NOTE: Only run after the above migrations for parent_guid and parent_tenant_id has been run
-- Delete triangulation partners of child shipments & consolidations for chain transfer

DELETE FROM triangulation_partner_shipment tps_child
USING (
    SELECT
        parent.id AS parent_id,
        child.id  AS child_id
    FROM shipment_details parent
    JOIN shipment_details child
        ON child.parent_guid = parent.guid
       AND parent.is_deleted = false
       AND child.is_deleted = false
) pcm
WHERE tps_child.shipment_id = pcm.child_id
  AND tps_child.partner_id IN (
      SELECT tps_parent.partner_id
      FROM triangulation_partner_shipment tps_parent
      WHERE tps_parent.shipment_id = pcm.parent_id
  );

DELETE FROM triangulation_partner_consolidation tpc_child
USING (
  SELECT
      parent.id AS parent_id,
      child.id  AS child_id
  FROM consolidation_details parent
  JOIN consolidation_details child
      ON child.parent_guid = parent.guid
     AND parent.is_deleted = false
     AND child.is_deleted = false
) pcm
WHERE tpc_child.consolidation_id = pcm.child_id
    AND tpc_child.partner_id IN (
        SELECT tpc_parent.partner_id
        FROM triangulation_partner_consolidation tpc_parent
        WHERE tpc_parent.consolidation_id = pcm.parent_id
    );