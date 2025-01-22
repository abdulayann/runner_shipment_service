CREATE INDEX IF NOT EXISTS shipment_source_guid_idx on shipment_details (source_guid);
CREATE INDEX IF NOT EXISTS shipment_guid_idx on shipment_details (guid);

CREATE INDEX IF NOT EXISTS consolidation_source_guid_idx on consolidation_details (source_guid);
CREATE INDEX IF NOT EXISTS consolidation_guid_idx on consolidation_details (guid);

CREATE INDEX IF NOT EXISTS containers_consolidation_id_idx on containers (consolidation_id);

CREATE INDEX IF NOT EXISTS hbl_shipment_id_idx on hbl (shipment_id);