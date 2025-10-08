CREATE UNIQUE INDEX unique_shipment_with_source_guid_idx
ON shipment_details (is_deleted, tenant_id, source_guid)
WHERE source_guid IS NOT NULL;


CREATE UNIQUE INDEX unique_consol_with_source_guid_idx
ON consolidation_details (is_deleted, tenant_id, source_guid)
WHERE source_guid IS NOT NULL;

