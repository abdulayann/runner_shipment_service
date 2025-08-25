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
CREATE INDEX IF NOT EXISTS idx_shipment_details_client_additional_carrier ON shipment_details (client_id, additional_details_id, carrier_detail_id) INCLUDE (is_deleted);
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

CREATE INDEX if not exists idx_consolidation_details_achieved_quantities_id ON consolidation_details (achieved_quantities_id);
CREATE INDEX if not exists idx_consolidation_details_allocations_id ON consolidation_details (allocations_id);
CREATE INDEX if not exists idx_consolidation_details_carrier_detail_id ON consolidation_details (carrier_detail_id);
CREATE INDEX if not exists idx_consolidation_details_borrowed_from_id ON consolidation_details (borrowed_from_id);
CREATE INDEX if not exists idx_consolidation_details_sending_agent_id ON consolidation_details (sending_agent_id);
CREATE INDEX if not exists idx_consolidation_details_receiving_agent_id ON consolidation_details (receiving_agent_id);
CREATE INDEX if not exists idx_consolidation_details_arrival_details_id ON consolidation_details (arrival_details_id);
CREATE INDEX if not exists idx_consolidation_details_co_load_with_id ON consolidation_details (co_load_with_id);
CREATE INDEX if not exists idx_consolidation_details_departure_details_id ON consolidation_details (departure_details_id);

CREATE INDEX IF NOT EXISTS idx_shipment_additional_details_ownership_org ON shipment_additional_details (ownership_org);
CREATE INDEX IF NOT EXISTS idx_shipment_additional_details_borrowed_from_id ON shipment_additional_details (borrowed_from_id);
CREATE INDEX IF NOT EXISTS idx_shipment_additional_details_receiving_agent_id ON shipment_additional_details (receiving_agent_id);
CREATE INDEX IF NOT EXISTS idx_shipment_additional_details_receiving_forwarder_id ON shipment_additional_details (receiving_forwarder_id);
CREATE INDEX IF NOT EXISTS idx_shipment_additional_details_sending_agent_id ON shipment_additional_details (sending_agent_id);
CREATE INDEX IF NOT EXISTS idx_shipment_additional_details_sending_forwarder_id ON shipment_additional_details (sending_forwarder_id);
CREATE INDEX IF NOT EXISTS idx_shipment_additional_details_trader_or_supplier_id ON shipment_additional_details (trader_or_supplier_id);
CREATE INDEX IF NOT EXISTS idx_shipment_additional_details_etailor_id ON shipment_additional_details (etailor_id);
CREATE INDEX IF NOT EXISTS idx_shipment_additional_details_export_broker_id ON shipment_additional_details (export_broker_id);
CREATE INDEX IF NOT EXISTS idx_shipment_additional_details_import_broker_id ON shipment_additional_details (import_broker_id);
CREATE INDEX IF NOT EXISTS idx_shipment_additional_details_notify_party_id ON shipment_additional_details (notify_party_id);

CREATE INDEX IF NOT EXISTS idx_containers_delivery_address_id ON containers (delivery_address_id);
CREATE INDEX IF NOT EXISTS idx_containers_pickup_address_id ON containers (pickup_address_id);
CREATE INDEX IF NOT EXISTS idx_jobs_buyer_detail_id ON jobs (buyer_detail_id);
CREATE INDEX IF NOT EXISTS idx_jobs_supplier_detail_id ON jobs (supplier_detail_id);
CREATE INDEX IF NOT EXISTS idx_pickup_delivery_details_agent_id ON pickup_delivery_details (agent_id);
CREATE INDEX IF NOT EXISTS idx_pickup_delivery_details_broker_id ON pickup_delivery_details (broker_id);
CREATE INDEX IF NOT EXISTS idx_pickup_delivery_details_destination_id ON pickup_delivery_details (destination_id);
CREATE INDEX IF NOT EXISTS idx_pickup_delivery_details_source_id ON pickup_delivery_details (source_id);
CREATE INDEX IF NOT EXISTS idx_pickup_delivery_details_transporter_id ON pickup_delivery_details (transporter_id);
CREATE INDEX IF NOT EXISTS idx_services_contractor_id ON services (contractor_id);

CREATE INDEX IF NOT EXISTS idx_truck_driver_details_container_id ON truck_driver_details (container_id);
CREATE INDEX IF NOT EXISTS idx_truck_driver_details_third_party_transporter ON truck_driver_details (third_party_transporter);
CREATE INDEX IF NOT EXISTS idx_product_sequence_config_tenant_products_id ON product_sequence_config (tenant_products_id);
CREATE INDEX IF NOT EXISTS idx_mawb_stocks_link_parent_id ON mawb_stocks_link (parent_id);
CREATE INDEX IF NOT EXISTS idx_customer_booking_carrier_detail_id ON customer_booking (carrier_detail_id);

CREATE INDEX IF NOT EXISTS idx_customer_booking_consignee_id ON customer_booking (consignee_id);
CREATE INDEX IF NOT EXISTS idx_customer_booking_consignor_id ON customer_booking (consignor_id);
CREATE INDEX IF NOT EXISTS idx_customer_booking_customer_id ON customer_booking (customer_id);
CREATE INDEX IF NOT EXISTS idx_customer_booking_notify_party_id ON customer_booking (notify_party_id);
CREATE INDEX IF NOT EXISTS idx_hbl_release_type_mapping_hbl_id ON hbl_release_type_mapping (hbl_id);

CREATE INDEX IF NOT EXISTS idx_security_status_shipment_id ON security_status (shipment_id);
CREATE INDEX IF NOT EXISTS idx_screening_status_shipment_additional_details_id ON screening_status (shipment_additional_details_id);
CREATE INDEX IF NOT EXISTS idx_qrtz_triggers_job_group ON qrtz_triggers (job_group);
CREATE INDEX IF NOT EXISTS idx_qrtz_triggers_job_name ON qrtz_triggers (job_name);
CREATE INDEX IF NOT EXISTS idx_dps_approval_detail_dps_event_id ON dps_approval_detail (dps_event_id);
CREATE INDEX IF NOT EXISTS idx_ti_legs_destination_id ON ti_legs (destination_id);
CREATE INDEX IF NOT EXISTS idx_ti_legs_origin_id ON ti_legs (origin_id);
CREATE INDEX IF NOT EXISTS idx_arrival_departure_details_last_foreign_port_id ON arrival_departure_details (last_foreign_port_id);
CREATE INDEX IF NOT EXISTS idx_arrival_departure_details_cto_id ON arrival_departure_details (cto_id);
CREATE INDEX IF NOT EXISTS idx_arrival_departure_details_transport_port_id ON arrival_departure_details (transport_port_id);
CREATE INDEX IF NOT EXISTS idx_arrival_departure_details_cfs_id ON arrival_departure_details (cfs_id);
CREATE INDEX IF NOT EXISTS idx_arrival_departure_details_first_foreign_port_id ON arrival_departure_details (first_foreign_port_id);
CREATE INDEX IF NOT EXISTS idx_arrival_departure_details_container_yard_id ON arrival_departure_details (container_yard_id);
