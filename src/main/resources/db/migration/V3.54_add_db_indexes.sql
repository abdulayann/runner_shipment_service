CREATE INDEX IF NOT EXISTS idx_notification_entity_type_id ON notification (entity_type, entity_id) INCLUDE (is_deleted);
CREATE INDEX IF NOT exists idx_screening_status_consol_consolidation_details_id ON screening_status_consol (consolidation_details_id);
CREATE INDEX IF NOT exists idx_network_transfer_tenant_entity ON network_transfer (tenant_id, entity_id) INCLUDE (is_deleted);
CREATE INDEX IF NOT exists idx_customer_booking_shipment_entity_id_v2 ON customer_booking (shipment_entity_id_v2) INCLUDE (is_deleted);
CREATE INDEX IF NOT EXISTS idx_consolidation_details_tenant_id ON consolidation_details (tenant_id) INCLUDE (is_deleted);
CREATE INDEX IF NOT exists idx_packing_container_id_is_deleted ON packing (container_id) INCLUDE (is_deleted);

CREATE INDEX IF NOT exists idx_services_shipment_id ON services (shipment_id) INCLUDE (is_deleted);
CREATE INDEX IF NOT EXISTS idx_shipments_containers_mapping_shipment_id ON shipments_containers_mapping (shipment_id) INCLUDE (is_deleted);
CREATE INDEX IF NOT exists idx_events_consolidation_id ON events (consolidation_id) INCLUDE (is_deleted);
CREATE INDEX IF NOT EXISTS idx_truck_driver_details_shipment_id ON truck_driver_details (shipment_id) INCLUDE (is_deleted);
CREATE INDEX IF NOT EXISTS idx_common_error_logs_entity_id ON common_error_logs (entity_id);

CREATE INDEX IF NOT EXISTS idx_shipment_details_container_assigned_to_shipment_cargo ON shipment_details (container_assigned_to_shipment_cargo) INCLUDE (is_deleted);
CREATE INDEX IF NOT EXISTS idx_customer_booking_tenant_id ON customer_booking (tenant_id) INCLUDE (is_deleted);

CREATE INDEX IF NOT EXISTS idx_reference_numbers_booking_id ON reference_numbers (booking_id) INCLUDE (is_deleted);
CREATE INDEX IF NOT EXISTS idx_ti_truck_driver_details_ti_leg_id ON ti_truck_driver_details (ti_leg_id) INCLUDE (is_deleted);
CREATE INDEX IF NOT EXISTS idx_ti_legs_pickup_delivery_details_id ON ti_legs (pickup_delivery_details_id) INCLUDE (is_deleted);
CREATE INDEX IF NOT EXISTS idx_screening_status_shipment_additional_details_id ON screening_status (shipment_additional_details_id);
CREATE INDEX IF NOT EXISTS idx_ti_reference_ti_leg_id ON ti_reference (ti_leg_id) INCLUDE (is_deleted) ;
CREATE INDEX IF NOT EXISTS idx_ti_containers_ti_leg_id ON ti_containers (ti_leg_id) INCLUDE (is_deleted);
CREATE INDEX IF NOT exists idx_tenant_products_tenant_id ON tenant_products (tenant_id) INCLUDE (is_deleted);
CREATE INDEX IF NOT EXISTS idx_reference_numbers_tenant_id ON reference_numbers (tenant_id) INCLUDE (is_deleted);
CREATE INDEX IF NOT EXISTS idx_shipment_setting_id ON hbl_terms_condition_template (shipment_settings_id) INCLUDE (is_deleted);
