-- NOTE: Only run after the BackfillParentGuidAndTenantIdForChainTransfer_253739.sql migrations has been run
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