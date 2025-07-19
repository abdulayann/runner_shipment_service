UPDATE public.awb SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.awb AS pawb
SET
    guid = sawb.guid,
    shipment_id = sawb.shipment_id,
    awb_shipment_info = sawb.awb_shipment_info,
    awb_notify_party_info = sawb.awb_notify_party_info,
    awb_routing_info = sawb.awb_routing_info,
    awb_cargo_info = sawb.awb_cargo_info,
    awb_payment_info = sawb.awb_payment_info,
    awb_other_charges_info = sawb.awb_other_charges_info,
    awb_other_info = sawb.awb_other_info,
    awb_oci_info = sawb.awb_oci_info,
    awb_goods_description_info = sawb.awb_goods_description_info,
    awb_packing_info = sawb.awb_packing_info,
    awb_special_handling_codes_mappings = sawb.awb_special_handling_codes_mappings,
    tenant_id = sawb.tenant_id,
    created_at = sawb.created_at,
    created_by = sawb.created_by,
    updated_at = sawb.updated_at,
    updated_by = sawb.updated_by,
    is_deleted = sawb.is_deleted, -- This will overwrite the previous is_deleted = TRUE if sawb.is_deleted is FALSE
    consolidation_id = sawb.consolidation_id,
    awb_number = sawb.awb_number,
    air_message_status = sawb.air_message_status,
    linked_hawb_air_message_status = sawb.linked_hawb_air_message_status,
    user_display_name = sawb.user_display_name,
    user_mail_id = sawb.user_mail_id,
    enable_fetch_rates_warning = sawb.enable_fetch_rates_warning,
    air_message_resubmitted = sawb.air_message_resubmitted,
    print_type = sawb.print_type,
    original_printed_at = sawb.original_printed_at,
    air_messaging_additional_fields = sawb.air_messaging_additional_fields,
    acas_enabled = sawb.acas_enabled,
    oci_info = sawb.oci_info
FROM __SCHEMA__.awb AS sawb
WHERE pawb.id = sawb.id AND sawb.tenant_id = __TENANT_ID__;





-- Table: public.doc_details
UPDATE public.doc_details SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.doc_details AS pdd
SET
    created_at = sdd.created_at,
    created_by = sdd.created_by,
    guid = sdd.guid,
    updated_at = sdd.updated_at,
    updated_by = sdd.updated_by,
    is_deleted = sdd.is_deleted, -- Overwrites previous is_deleted
    tenant_id = sdd.tenant_id,
    version_number = sdd.version_number,
    "type" = sdd."type",
    entity_id = sdd.entity_id
FROM __SCHEMA__.doc_details AS sdd
WHERE pdd.id = sdd.id AND sdd.tenant_id = __TENANT_ID__;




-- Table: public.hbl
UPDATE public.hbl SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.hbl AS phbl
SET
    created_at = shbl.created_at,
    created_by = shbl.created_by,
    guid = shbl.guid,
    updated_at = shbl.updated_at,
    updated_by = shbl.updated_by,
    tenant_id = shbl.tenant_id,
    is_deleted = shbl.is_deleted, -- Overwrites previous is_deleted
    hbl_data = shbl.hbl_data,
    container_data = shbl.container_data,
    cargo_data = shbl.cargo_data,
    notify_party_data = shbl.notify_party_data,
    shipment_id = shbl.shipment_id
FROM __SCHEMA__.hbl AS shbl
WHERE phbl.id = shbl.id AND shbl.tenant_id = __TENANT_ID__;




-- Table: public.hbl_release_type_mapping
UPDATE public.hbl_release_type_mapping SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.hbl_release_type_mapping AS phrtm
SET
    guid = shrtm.guid,
    tenant_id = shrtm.tenant_id,
    hbl_id = shrtm.hbl_id,
    release_type = shrtm.release_type,
    copies_printed = shrtm.copies_printed,
    created_at = shrtm.created_at,
    updated_at = shrtm.updated_at,
    created_by = shrtm.created_by,
    updated_by = shrtm.updated_by,
    is_deleted = shrtm.is_deleted -- Overwrites previous is_deleted
FROM __SCHEMA__.hbl_release_type_mapping AS shrtm
WHERE phrtm.id = shrtm.id AND shrtm.tenant_id = __TENANT_ID__;




-- Table: public.mawb_hawb_link
UPDATE public.mawb_hawb_link SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.mawb_hawb_link AS pmhl
SET
    guid = smhl.guid,
    mawb_id = smhl.mawb_id,
    hawb_id = smhl.hawb_id,
    tenant_id = smhl.tenant_id,
    created_at = smhl.created_at,
    created_by = smhl.created_by,
    updated_at = smhl.updated_at,
    updated_by = smhl.updated_by,
    is_deleted = smhl.is_deleted -- Overwrites previous is_deleted
FROM __SCHEMA__.mawb_hawb_link AS smhl
WHERE pmhl.id = smhl.id AND smhl.tenant_id = __TENANT_ID__;




-- Table: public.mawb_stocks_link
UPDATE public.mawb_stocks_link SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.mawb_stocks_link AS pmsl
SET
    guid = smsl.guid,
    parent_id = smsl.parent_id,
    mawb_number = smsl.mawb_number,
    status = smsl.status,
    seq_number = smsl.seq_number,
    entity_type = smsl.entity_type,
    entity_id = smsl.entity_id,
    ship_cons_number = smsl.ship_cons_number,
    tenant_id = smsl.tenant_id,
    created_at = smsl.created_at,
    created_by = smsl.created_by,
    updated_at = smsl.updated_at,
    updated_by = smsl.updated_by,
    is_deleted = smsl.is_deleted -- Overwrites previous is_deleted
FROM __SCHEMA__.mawb_stocks_link AS smsl
WHERE pmsl.id = smsl.id AND smsl.tenant_id = __TENANT_ID__;




-- Table: public.mawb_stocks
UPDATE public.mawb_stocks SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.mawb_stocks AS pms
SET
    guid = sms.guid,
    consolidation_id = sms.consolidation_id,
    mawb_number = sms.mawb_number,
    next_mawb_number = sms.next_mawb_number,
    available_count = sms.available_count,
    status = sms.status,
    home_port = sms.home_port,
    air_line_prefix = sms.air_line_prefix,
    prefix = sms.prefix,
    seq_number = sms.seq_number,
    count = sms.count,
    start_number = sms.start_number,
    mawb_stocks_from = sms.mawb_stocks_from,
    mawb_stocks_to = sms.mawb_stocks_to,
    borrowed_from = sms.borrowed_from,
    borrowed_from_full_name = sms.borrowed_from_full_name,
    tenant_id = sms.tenant_id,
    created_at = sms.created_at,
    created_by = sms.created_by,
    updated_at = sms.updated_at,
    updated_by = sms.updated_by,
    is_deleted = sms.is_deleted -- Overwrites previous is_deleted
FROM __SCHEMA__.mawb_stocks AS sms
WHERE pms.id = sms.id AND sms.tenant_id = __TENANT_ID__;




-- Table: public.notification
UPDATE public.notification SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.notification AS pn
SET
    created_at = sn.created_at,
    created_by = sn.created_by,
    guid = sn.guid,
    updated_at = sn.updated_at,
    updated_by = sn.updated_by,
    is_deleted = sn.is_deleted, -- Overwrites previous is_deleted
    tenant_id = sn.tenant_id,
    entity_type = sn.entity_type,
    entity_id = sn.entity_id,
    requested_branch_id = sn.requested_branch_id,
    requested_user = sn.requested_user,
    requested_on = sn.requested_on,
    request_type = sn.request_type,
    reason = sn.reason,
    reassigned_to_branch_id = sn.reassigned_to_branch_id,
    reassigned_from_branch_id = sn.reassigned_from_branch_id,
    network_transfer_id = sn.network_transfer_id
FROM __SCHEMA__.notification AS sn
WHERE pn.id = sn.id AND sn.tenant_id = __TENANT_ID__;





-- Table: public.ti_truck_driver_details
UPDATE public.ti_truck_driver_details SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.ti_truck_driver_details AS pttd
SET
    created_at = sttd.created_at,
    created_by = sttd.created_by,
    guid = sttd.guid,
    updated_at = sttd.updated_at,
    updated_by = sttd.updated_by,
    is_deleted = sttd.is_deleted, -- Overwrites previous is_deleted
    tenant_id = sttd.tenant_id,
    ti_leg_id = sttd.ti_leg_id,
    driver_name = sttd.driver_name,
    driver_mobile_number = sttd.driver_mobile_number,
    truck_number_plate = sttd.truck_number_plate,
    trailer_number_plate = sttd.trailer_number_plate,
    truck_or_trailer_type = sttd.truck_or_trailer_type,
    driver_id = sttd.driver_id
FROM __SCHEMA__.ti_truck_driver_details AS sttd
WHERE pttd.id = sttd.id AND sttd.tenant_id = __TENANT_ID__;





-- Table: public.ti_reference
UPDATE public.ti_reference SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.ti_reference AS ptr
SET
    created_at = str.created_at,
    created_by = str.created_by,
    guid = str.guid,
    updated_at = str.updated_at,
    updated_by = str.updated_by,
    is_deleted = str.is_deleted, -- Overwrites previous is_deleted
    tenant_id = str.tenant_id,
    ti_leg_id = str.ti_leg_id,
    "type" = str."type",
    reference = str.reference
FROM __SCHEMA__.ti_reference AS str
WHERE ptr.id = str.id AND str.tenant_id = __TENANT_ID__;





-- Table: public.ti_packages
UPDATE public.ti_packages SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.ti_packages AS ptp
SET
    created_at = stp.created_at,
    created_by = stp.created_by,
    guid = stp.guid,
    updated_at = stp.updated_at,
    updated_by = stp.updated_by,
    is_deleted = stp.is_deleted, -- Overwrites previous is_deleted
    tenant_id = stp.tenant_id,
    ti_leg_id = stp.ti_leg_id,
    no_of_packages = stp.no_of_packages,
    package_type = stp.package_type,
    description = stp.description,
    dimensions = stp.dimensions,
    gross_weight = stp.gross_weight,
    gross_weight_unit = stp.gross_weight_unit,
    net_weight = stp.net_weight,
    net_weight_unit = stp.net_weight_unit,
    volume = stp.volume,
    volume_unit = stp.volume_unit,
    dangerous = stp.dangerous,
    substance_name = stp.substance_name,
    un_number = stp.un_number,
    hazard_label = stp.hazard_label,
    tunnel_restriction_code = stp.tunnel_restriction_code,
    length_unit = stp.length_unit,
    width_unit = stp.width_unit,
    height_unit = stp.height_unit,
    length = stp.length,
    width = stp.width,
    height = stp.height
FROM __SCHEMA__.ti_packages AS stp
WHERE ptp.id = stp.id AND stp.tenant_id = __TENANT_ID__;






-- Table: public.ti_containers
UPDATE public.ti_containers SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.ti_containers AS ptc
SET
    created_at = stc.created_at,
    created_by = stc.created_by,
    guid = stc.guid,
    updated_at = stc.updated_at,
    updated_by = stc.updated_by,
    is_deleted = stc.is_deleted, -- Overwrites previous is_deleted
    tenant_id = stc.tenant_id,
    ti_leg_id = stc.ti_leg_id,
    "type" = stc."type",
    "number" = stc."number",
    description = stc.description,
    no_of_packages = stc.no_of_packages,
    gross_weight = stc.gross_weight,
    gross_weight_unit = stc.gross_weight_unit,
    net_weight = stc.net_weight,
    net_weight_unit = stc.net_weight_unit,
    volume = stc.volume,
    volume_unit = stc.volume_unit,
    dangerous = stc.dangerous,
    substance_name = stc.substance_name,
    un_number = stc.un_number,
    dg_class = stc.dg_class,
    tunnel_restriction_code = stc.tunnel_restriction_code,
    package_type = stc.package_type
FROM __SCHEMA__.ti_containers AS stc
WHERE ptc.id = stc.id AND stc.tenant_id = __TENANT_ID__;





-- Table: public.ti_legs
UPDATE public.ti_legs SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.ti_legs AS ptl
SET
    created_at = stl.created_at,
    created_by = stl.created_by,
    guid = stl.guid,
    updated_at = stl.updated_at,
    updated_by = stl.updated_by,
    is_deleted = stl.is_deleted, -- Overwrites previous is_deleted
    tenant_id = stl.tenant_id,
    "sequence" = stl."sequence",
    leg_type = stl.leg_type,
    origin_id = stl.origin_id,
    destination_id = stl.destination_id,
    estimated_pickup = stl.estimated_pickup,
    estimated_delivery = stl.estimated_delivery,
    actual_pickup = stl.actual_pickup,
    actual_delivery = stl.actual_delivery,
    required_by = stl.required_by,
    drop_mode = stl.drop_mode,
    remarks = stl.remarks,
    pickup_delivery_details_id = stl.pickup_delivery_details_id
FROM __SCHEMA__.ti_legs AS stl
WHERE ptl.id = stl.id AND stl.tenant_id = __TENANT_ID__;





-- Table: public.jobs
UPDATE public.jobs SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.jobs AS pj
SET
    created_at = sj.created_at,
    created_by = sj.created_by,
    guid = sj.guid,
    updated_at = sj.updated_at,
    updated_by = sj.updated_by,
    tenant_id = sj.tenant_id,
    additional_terms = sj.additional_terms,
    buyer_id = sj.buyer_id,
    confirm_date = sj.confirm_date,
    confirm_number = sj.confirm_number,
    consolidation_id = sj.consolidation_id,
    country_of_origin = sj.country_of_origin,
    currency = sj.currency,
    description = sj.description,
    follow_up_date = sj.follow_up_date,
    inco_term = sj.inco_term,
    invoice_date = sj.invoice_date,
    invoice_number = sj.invoice_number,
    order_date = sj.order_date,
    order_number = sj.order_number,
    order_status = sj.order_status,
    service_mode = sj.service_mode,
    shipment_id = sj.shipment_id,
    transport_mode = sj.transport_mode,
    buyer_detail_id = sj.buyer_detail_id,
    supplier_detail_id = sj.supplier_detail_id,
    is_deleted = sj.is_deleted -- Overwrites previous is_deleted
FROM __SCHEMA__.jobs AS sj
WHERE pj.id = sj.id AND sj.tenant_id = __TENANT_ID__;





-- Table: public.notes
UPDATE public.notes SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.notes AS pn
SET
    created_at = sn.created_at,
    created_by = sn.created_by,
    guid = sn.guid,
    updated_at = sn.updated_at,
    updated_by = sn.updated_by,
    tenant_id = sn.tenant_id,
    entity_id = sn.entity_id,
    entity_type = sn.entity_type,
    insert_date = sn.insert_date,
    insert_user_display_name = sn.insert_user_display_name,
    insert_user_id = sn.insert_user_id,
    is_active = sn.is_active,
    is_public = sn.is_public,
    "text" = sn."text",
    is_deleted = sn.is_deleted, -- Overwrites previous is_deleted
    "label" = sn."label",
    assigned_to = sn.assigned_to,
    is_read_only = sn.is_read_only
FROM __SCHEMA__.notes AS sn
WHERE pn.id = sn.id AND sn.tenant_id = __TENANT_ID__;




-- Table: public.shipment_additional_details
UPDATE public.shipment_additional_details SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.shipment_additional_details AS psad
SET
    created_at = ssad.created_at,
    created_by = ssad.created_by,
    guid = ssad.guid,
    updated_at = ssad.updated_at,
    updated_by = ssad.updated_by,
    tenant_id = ssad.tenant_id,
    ad_code = ssad.ad_code,
    be_type = ssad.be_type,
    bl_charges_display = ssad.bl_charges_display,
    bl_exporter_shipment = ssad.bl_exporter_shipment,
    boe_date = ssad.boe_date,
    boe_number = ssad.boe_number,
    cha_job_number = ssad.cha_job_number,
    cif_value = ssad.cif_value,
    ie_code = ssad.ie_code,
    igm_file_date = ssad.igm_file_date,
    igm_file_no = ssad.igm_file_no,
    igm_inward_date = ssad.igm_inward_date,
    smtp_igm_date = ssad.smtp_igm_date,
    smtp_igm_number = ssad.smtp_igm_number,
    wbl_printed = ssad.wbl_printed,
    activity_type = ssad.activity_type,
    airway_bill_dims = ssad.airway_bill_dims,
    andes_response_date = ssad.andes_response_date,
    andes_status = ssad.andes_status,
    andes_status_response_text = ssad.andes_status_response_text,
    andes_ticket = ssad.andes_ticket,
    assess_value = ssad.assess_value,
    bonded_warehouse_id = ssad.bonded_warehouse_id,
    branch_si_number = ssad.branch_si_number,
    "copy" = ssad."copy",
    custom_city = ssad.custom_city,
    custom_house = ssad.custom_house,
    custom_location = ssad.custom_location,
    customs_no_issue_date = ssad.customs_no_issue_date,
    date_of_issue = ssad.date_of_issue,
    date_of_receipt = ssad.date_of_receipt,
    delivery_mode_id = ssad.delivery_mode_id,
    draft_printed = ssad.draft_printed,
    efreight_status = ssad.efreight_status,
    expiry_date = ssad.expiry_date,
    external_notes = ssad.external_notes,
    free_days = ssad.free_days,
    goods_co_id = ssad.goods_co_id,
    house_bill_type = ssad.house_bill_type,
    hsn_number = ssad.hsn_number,
    import_export_shipment_lock = ssad.import_export_shipment_lock,
    inspection = ssad.inspection,
    invoice_value = ssad.invoice_value,
    inward_date_and_time = ssad.inward_date_and_time,
    is_cms_hbl_sent = ssad.is_cms_hbl_sent,
    is_credit_override_approved = ssad.is_credit_override_approved,
    is_export_clearance = ssad.is_export_clearance,
    is_import_clearance = ssad.is_import_clearance,
    is_inland = ssad.is_inland,
    lgd_status = ssad.lgd_status,
    line_number = ssad.line_number,
    local_line_number = ssad.local_line_number,
    on_board = ssad.on_board,
    on_board_date = ssad.on_board_date,
    original = ssad.original,
    ownership = ssad.ownership,
    ownership_name = ssad.ownership_name,
    paid_place = ssad.paid_place,
    passed_by = ssad.passed_by,
    passed_by_person = ssad.passed_by_person,
    peru_entry_exit_point = ssad.peru_entry_exit_point,
    phase = ssad.phase,
    place_of_supply = ssad.place_of_supply,
    printed_original = ssad.printed_original,
    release_type = ssad.release_type,
    screening_status = ssad.screening_status,
    shipment_id = ssad.shipment_id,
    shipper_cod = ssad.shipper_cod,
    shipper_cod_pm = ssad.shipper_cod_pm,
    spot_rate = ssad.spot_rate,
    spot_rate_type = ssad.spot_rate_type,
    sub_line_number = ssad.sub_line_number,
    supplier_invoice_date = ssad.supplier_invoice_date,
    supplier_invoice_number = ssad.supplier_invoice_number,
    surrender_printed = ssad.surrender_printed,
    tipo_document_consignee = ssad.tipo_document_consignee,
    tipo_document_consignor = ssad.tipo_document_consignor,
    tipo_document_notify_party = ssad.tipo_document_notify_party,
    total_duty = ssad.total_duty,
    warehouse_id = ssad.warehouse_id,
    borrowed_from_id = ssad.borrowed_from_id,
    etailor_id = ssad.etailor_id,
    export_broker_id = ssad.export_broker_id,
    import_broker_id = ssad.import_broker_id,
    notify_party_id = ssad.notify_party_id,
    receiving_agent_id = ssad.receiving_agent_id,
    receiving_forwarder_id = ssad.receiving_forwarder_id,
    sending_agent_id = ssad.sending_agent_id,
    sending_forwarder_id = ssad.sending_forwarder_id,
    trader_or_supplier_id = ssad.trader_or_supplier_id,
    is_deleted = ssad.is_deleted, -- Overwrites previous is_deleted
    place_of_issue = ssad.place_of_issue,
    ownership_org = ssad.ownership_org,
    custom_decl_type = ssad.custom_decl_type,
    agent_reference = ssad.agent_reference,
    bl_terms_and_conditions_id = ssad.bl_terms_and_conditions_id,
    bl_comments = ssad.bl_comments,
    cargo_terms = ssad.cargo_terms,
    cargo_terms_description = ssad.cargo_terms_description,
    bl_remarks = ssad.bl_remarks,
    bl_remarks_description = ssad.bl_remarks_description,
    summary = ssad.summary,
    is_summary_updated = ssad.is_summary_updated,
    exemption_codes = ssad.exemption_codes,
    aom_free_text = ssad.aom_free_text,
    emergency_contact_number = ssad.emergency_contact_number,
    emergency_contact_number_code = ssad.emergency_contact_number_code,
    sci = ssad.sci,
    pickup_date = ssad.pickup_date,
    cargo_delivered_date = ssad.cargo_delivered_date,
    custom_release_date = ssad.custom_release_date,
    doc_turned_over_to_customer = ssad.doc_turned_over_to_customer,
    proof_of_delivery_date = ssad.proof_of_delivery_date,
    warehouse_cargo_arrival_date = ssad.warehouse_cargo_arrival_date,
    pickup_by_consignee_completed = ssad.pickup_by_consignee_completed,
    empty_container_returned = ssad.empty_container_returned,
    security_status_received_from = ssad.security_status_received_from,
    additional_security_information = ssad.additional_security_information,
    regulated_entity_category = ssad.regulated_entity_category,
    is_export_custom_clearance_completed = ssad.is_export_custom_clearance_completed,
    bl_instruction_received = ssad.bl_instruction_received,
    cargo_out_for_delivery = ssad.cargo_out_for_delivery,
    import_broker_country = ssad.import_broker_country,
    export_broker_country = ssad.export_broker_country,
    fcr_number = ssad.fcr_number
FROM __SCHEMA__.shipment_additional_details AS ssad
WHERE psad.id = ssad.id AND ssad.tenant_id = __TENANT_ID__;






-- Table: public.booking_carriage
UPDATE public.booking_carriage SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.booking_carriage AS pbc
SET
    created_at = sbc.created_at,
    created_by = sbc.created_by,
    guid = sbc.guid,
    updated_at = sbc.updated_at,
    updated_by = sbc.updated_by,
    tenant_id = sbc.tenant_id,
    carriage_mode = sbc.carriage_mode,
    carriage_type = sbc.carriage_type,
    eta = sbc.eta,
    etd = sbc.etd,
    shipment_id = sbc.shipment_id,
    vessel = sbc.vessel,
    voyage = sbc.voyage,
    is_deleted = sbc.is_deleted, -- Overwrites previous is_deleted
    port_of_loading = sbc.port_of_loading,
    port_of_discharge = sbc.port_of_discharge
FROM __SCHEMA__.booking_carriage AS sbc
WHERE pbc.id = sbc.id AND sbc.tenant_id = __TENANT_ID__;






-- Table: public.console_shipment_mapping
UPDATE public.console_shipment_mapping
SET is_deleted = TRUE
WHERE shipment_id IN (
    SELECT id FROM public.shipment_details WHERE tenant_id = __TENANT_ID__
);

UPDATE public.console_shipment_mapping AS pcsm
SET
    id = scsm.id,
    guid = scsm.guid,
    consolidation_id = scsm.consolidation_id, -- Added
    shipment_id = scsm.shipment_id,           -- Added
    created_at = scsm.created_at,
    updated_at = scsm.updated_at,
    created_by = scsm.created_by,
    updated_by = scsm.updated_by,
    is_deleted = scsm.is_deleted,
    is_attachment_done = scsm.is_attachment_done,
    request_type = scsm.request_type
FROM __SCHEMA__.console_shipment_mapping AS scsm
WHERE pcsm.id = scsm.id AND scsm.shipment_id IN (SELECT id FROM __SCHEMA__.shipment_details WHERE tenant_id = __TENANT_ID__);





-- Table: public.shipments_containers_mapping
UPDATE public.shipments_containers_mapping
SET is_deleted = TRUE
WHERE shipment_id IN (
    SELECT id FROM public.shipment_details WHERE tenant_id = __TENANT_ID__
);
UPDATE public.shipments_containers_mapping AS pscm
SET
    guid = sscm.guid,
    container_id = sscm.container_id,
    shipment_id = sscm.shipment_id,
    created_at = sscm.created_at,
    updated_at = sscm.updated_at,
    created_by = sscm.created_by,
    updated_by = sscm.updated_by,
    is_deleted = sscm.is_deleted -- Overwrites previous is_deleted
FROM __SCHEMA__.shipments_containers_mapping AS sscm
WHERE pscm.id = sscm.id AND sscm.shipment_id IN (SELECT id FROM __SCHEMA__.shipment_details WHERE tenant_id = __TENANT_ID__);






-- Table: public.el_details
UPDATE public.el_details SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.el_details AS ped
SET
    created_at = sed.created_at,
    created_by = sed.created_by,
    guid = sed.guid,
    updated_at = sed.updated_at,
    updated_by = sed.updated_by,
    tenant_id = sed.tenant_id,
    el_number = sed.el_number,
    merge_class = sed.merge_class,
    merge_package_unit = sed.merge_package_unit,
    merge_package = sed.merge_package,
    packages = sed.packages,
    "partition" = sed."partition",
    partition_seq_number = sed.partition_seq_number,
    shipment_id = sed.shipment_id,
    unit = sed.unit,
    weight = sed.weight,
    weight_unit = sed.weight_unit,
    is_deleted = sed.is_deleted -- Overwrites previous is_deleted
FROM __SCHEMA__.el_details AS sed
WHERE ped.id = sed.id AND sed.tenant_id = __TENANT_ID__;






-- Table: public.events
UPDATE public.events SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.events AS pe
SET
    created_at = se.created_at,
    created_by = se.created_by,
    guid = se.guid,
    updated_at = se.updated_at,
    updated_by = se.updated_by,
    tenant_id = se.tenant_id,
    actual = se.actual,
    description = se.description,
    estimated = se.estimated,
    event_estimate_update_reasons = se.event_estimate_update_reasons,
    is_public_tracking_event = se.is_public_tracking_event,
    latitude = se.latitude,
    longitude = se.longitude,
    event_code = se.event_code,
    place_description = se.place_description,
    place_name = se.place_name,
    "source" = se."source",
    is_deleted = se.is_deleted, -- Overwrites previous is_deleted
    entity_id = se.entity_id,
    entity_type = se.entity_type,
    status = se.status,
    pieces = se.pieces,
    total_pieces = se.total_pieces,
    weight = se.weight,
    total_weight = se.total_weight,
    received_date = se.received_date,
    scheduled_date = se.scheduled_date,
    is_partial = se.is_partial,
    container_number = se.container_number,
    location_role = se.location_role,
    consolidation_id = se.consolidation_id,
    shipment_number = se.shipment_number,
    flight_number = se.flight_number,
    flight_name = se.flight_name,
    direction = se.direction,
    event_type = se.event_type,
    remarks = se.remarks,
    user_name = se.user_name,
    user_email = se.user_email,
    branch = se.branch,
    reference_number = se.reference_number,
    branch_name = se.branch_name
FROM __SCHEMA__.events AS se
WHERE pe.id = se.id AND se.tenant_id = __TENANT_ID__;






-- Table: public.packing
UPDATE public.packing SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.packing AS pp
SET
    created_at = sp.created_at,
    created_by = sp.created_by,
    guid = sp.guid,
    updated_at = sp.updated_at,
    updated_by = sp.updated_by,
    tenant_id = sp.tenant_id,
    dg_class_id = sp.dg_class_id,
    dg_goods_id = sp.dg_goods_id,
    dg_substance_id = sp.dg_substance_id,
    hs_code = sp.hs_code,
    undg_contact = sp.undg_contact,
    chargeable = sp.chargeable,
    chargeable_unit = sp.chargeable_unit,
    commodity = sp.commodity,
    commodity_id = sp.commodity_id,
    consolidation_id = sp.consolidation_id,
    container_id = sp.container_id,
    container_number = sp.container_number,
    country_code = sp.country_code,
    customs_release_code = sp.customs_release_code,
    flash_point = sp.flash_point,
    goods_description = sp.goods_description,
    hazardous = sp.hazardous,
    height = sp.height,
    height_unit = sp.height_unit,
    inner_package_number = sp.inner_package_number,
    inner_package_type_id = sp.inner_package_type_id,
    inner_packs_count = sp.inner_packs_count,
    inner_packs_id = sp.inner_packs_id,
    inspections = sp.inspections,
    is_temperature_controlled = sp.is_temperature_controlled,
    length = sp.length,
    length_unit = sp.length_unit,
    marksn_nums = sp.marksn_nums,
    max_temp = sp.max_temp,
    max_temp_unit = sp.max_temp_unit,
    min_temp = sp.min_temp,
    min_temp_unit_id = sp.min_temp_unit_id,
    net_weight = sp.net_weight,
    net_weight_unit = sp.net_weight_unit,
    origin = sp.origin,
    packing_order = sp.packing_order,
    packs = sp.packs,
    packs_type = sp.packs_type,
    reference_number = sp.reference_number,
    shipment_id = sp.shipment_id,
    shipment_number = sp.shipment_number,
    transport_mode = sp.transport_mode,
    vin_number = sp.vin_number,
    volume = sp.volume,
    volume_unit = sp.volume_unit,
    volume_weight = sp.volume_weight,
    volume_weight_unit = sp.volume_weight_unit,
    weight = sp.weight,
    weight_unit = sp.weight_unit,
    width = sp.width,
    width_unit = sp.width_unit,
    is_deleted = sp.is_deleted, -- Overwrites previous is_deleted
    booking_id = sp.booking_id,
    commodity_group = sp.commodity_group,
    is_dimension = sp.is_dimension,
    is_contract_enforced = sp.is_contract_enforced,
    handling_info = sp.handling_info,
    contract_enforced_quantity_limit = sp.contract_enforced_quantity_limit,
    un_number_air = sp.un_number_air,
    dg_class_air = sp.dg_class_air,
    dg_class_air_description = sp.dg_class_air_description,
    cargo_gate_in_date = sp.cargo_gate_in_date,
    date_type = sp.date_type,
    un_number = sp.un_number,
    proper_shipping_name = sp.proper_shipping_name,
    packing_group = sp.packing_group,
    minimum_flash_point = sp.minimum_flash_point,
    minimum_flash_point_unit = sp.minimum_flash_point_unit,
    marine_pollutant = sp.marine_pollutant
FROM __SCHEMA__.packing AS sp
WHERE pp.id = sp.id AND sp.tenant_id = __TENANT_ID__;






-- Table: public.reference_numbers
UPDATE public.reference_numbers SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.reference_numbers AS prn
SET
    created_at = srn.created_at,
    created_by = srn.created_by,
    guid = srn.guid,
    updated_at = srn.updated_at,
    updated_by = srn.updated_by,
    tenant_id = srn.tenant_id,
    consolidation_id = srn.consolidation_id,
    country_of_issue = srn.country_of_issue,
    reference_number = srn.reference_number,
    shipment_id = srn.shipment_id,
    "type" = srn."type",
    is_deleted = srn.is_deleted, -- Overwrites previous is_deleted
    is_portal_enable = srn.is_portal_enable,
    booking_id = srn.booking_id
FROM __SCHEMA__.reference_numbers AS srn
WHERE prn.id = srn.id AND srn.tenant_id = __TENANT_ID__;




-- Table: public.routings
UPDATE public.routings SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.routings AS pr
SET
    created_at = sr.created_at,
    created_by = sr.created_by,
    guid = sr.guid,
    updated_at = sr.updated_at,
    updated_by = sr.updated_by,
    tenant_id = sr.tenant_id,
    aircraft_registration = sr.aircraft_registration,
    aircraft_type = sr.aircraft_type,
    ata = sr.ata,
    atd = sr.atd,
    consolidation_id = sr.consolidation_id,
    eta = sr.eta,
    etd = sr.etd,
    flight_number = sr.flight_number,
    is_domestic = sr.is_domestic,
    is_linked = sr.is_linked,
    leg = sr.leg,
    "mode" = sr."mode",
    route_leg_id = sr.route_leg_id,
    routing_status = sr.routing_status,
    shipment_id = sr.shipment_id,
    transit_days = sr.transit_days,
    vessel_id = sr.vessel_id,
    vessel_name = sr.vessel_name,
    voyage = sr.voyage,
    is_deleted = sr.is_deleted, -- Overwrites previous is_deleted
    carrier = sr.carrier,
    pol = sr.pol,
    pod = sr.pod,
    booking_id = sr.booking_id,
    truck_reference_number = sr.truck_reference_number,
    carrier_country = sr.carrier_country,
    is_selected_for_document = sr.is_selected_for_document,
    vehicle_number = sr.vehicle_number,
    carriage = sr.carriage,
    origin_port_loc_code = sr.origin_port_loc_code,
    destination_port_loc_code = sr.destination_port_loc_code,
    inherited_from_consolidation = sr.inherited_from_consolidation
FROM __SCHEMA__.routings AS sr
WHERE pr.id = sr.id AND sr.tenant_id = __TENANT_ID__;






-- Table: public.services
UPDATE public.services SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.services AS ps
SET
    created_at = ss.created_at,
    created_by = ss.created_by,
    guid = ss.guid,
    updated_at = ss.updated_at,
    updated_by = ss.updated_by,
    tenant_id = ss.tenant_id,
    booking_date = ss.booking_date,
    completion_date = ss.completion_date,
    consolidation_id = ss.consolidation_id,
    ref_number = ss.ref_number,
    service_count = ss.service_count,
    service_notes = ss.service_notes,
    service_type = ss.service_type,
    shipment_id = ss.shipment_id,
    srv_location = ss.srv_location,
    contractor_id = ss.contractor_id,
    is_deleted = ss.is_deleted, -- Overwrites previous is_deleted
    service_duration = ss.service_duration
FROM __SCHEMA__.services AS ss
WHERE ps.id = ss.id AND ss.tenant_id = __TENANT_ID__;





-- Table: public.truck_driver_details
UPDATE public.truck_driver_details SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.truck_driver_details AS ptd
SET
    created_at = std.created_at,
    created_by = std.created_by,
    guid = std.guid,
    updated_at = std.updated_at,
    updated_by = std.updated_by,
    tenant_id = std.tenant_id,
    consolidation_id = std.consolidation_id,
    container_id = std.container_id,
    container_type_code = std.container_type_code,
    driver_mobile_number = std.driver_mobile_number,
    driver_name = std.driver_name,
    self_transporter_name = std.self_transporter_name,
    shipment_id = std.shipment_id,
    trailer_number_plate = std.trailer_number_plate,
    transporter_name = std.transporter_name,
    transporter_type = std.transporter_type,
    truck_number_plate = std.truck_number_plate,
    truck_or_trailer_type_id = std.truck_or_trailer_type_id,
    is_deleted = std.is_deleted, -- Overwrites previous is_deleted
    remarks = std.remarks,
    truck_status = std.truck_status,
    third_party_transporter = std.third_party_transporter,
    driver_id = std.driver_id
FROM __SCHEMA__.truck_driver_details AS std
WHERE ptd.id = std.id AND std.tenant_id = __TENANT_ID__;






-- Table: public.shipment_details
UPDATE public.shipment_details SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.shipment_details AS psd
SET
    created_at = ssd.created_at,
    created_by = ssd.created_by,
    guid = ssd.guid,
    updated_at = ssd.updated_at,
    updated_by = ssd.updated_by,
    tenant_id = ssd.tenant_id,
    source_tenant_id = ssd.source_tenant_id,
    additional_terms = ssd.additional_terms,
    assigned_to = ssd.assigned_to,
    auto_update_wt_vol = ssd.auto_update_wt_vol,
    booking_number = ssd.booking_number,
    booking_reference = ssd.booking_reference,
    booking_type = ssd.booking_type,
    cargo_finance_booking = ssd.cargo_finance_booking,
    chargable = ssd.chargable,
    chargeable_unit = ssd.chargeable_unit,
    console_ref = ssd.console_ref,
    container_auto_wv_update = ssd.container_auto_wv_update,
    direction = ssd.direction,
    documentation_partner = ssd.documentation_partner,
    entry_detail = ssd.entry_detail,
    finance_closed_by = ssd.finance_closed_by,
    finance_closed_on = ssd.finance_closed_on,
    freight_local = ssd.freight_local,
    freight_local_currency = ssd.freight_local_currency,
    freight_overseas = ssd.freight_overseas,
    freight_overseas_currency = ssd.freight_overseas_currency,
    goods_description = ssd.goods_description,
    house_bill = ssd.house_bill,
    incoterms = ssd.incoterms,
    inner_pack_unit = ssd.inner_pack_unit,
    inner_packs = ssd.inner_packs,
    intra_branch = ssd.intra_branch,
    is_domestic = ssd.is_domestic,
    is_locked = ssd.is_locked,
    is_notify_consignee_equal = ssd.is_notify_consignee_equal,
    is_shipment_read_only = ssd.is_shipment_read_only,
    job_type = ssd.job_type,
    locked_by = ssd.locked_by,
    marks_num = ssd.marks_num,
    master_bill = ssd.master_bill,
    net_weight = ssd.net_weight,
    net_weight_unit = ssd.net_weight_unit,
    no_of_packs = ssd.no_of_packs,
    packs_unit = ssd.packs_unit,
    payment_terms = ssd.payment_terms,
    prev_shipment_status = ssd.prev_shipment_status,
    receiving_branch = ssd.receiving_branch,
    route = ssd.route,
    sales_agent = ssd.sales_agent,
    service_type = ssd.service_type,
    shipment_completed_by = ssd.shipment_completed_by,
    shipment_completed_on = ssd.shipment_completed_on,
    shipment_id = ssd.shipment_id,
    shipment_type = ssd.shipment_type,
    "source" = ssd."source",
    status = ssd.status,
    transport_mode = ssd.transport_mode,
    volume = ssd.volume,
    volume_unit = ssd.volume_unit,
    volumetric_weight = ssd.volumetric_weight,
    volumetric_weight_unit = ssd.volumetric_weight_unit,
    weight = ssd.weight,
    weight_unit = ssd.weight_unit,
    additional_details_id = ssd.additional_details_id,
    carrier_detail_id = ssd.carrier_detail_id,
    client_id = ssd.client_id,
    consignee_id = ssd.consignee_id,
    consigner_id = ssd.consigner_id,
    is_deleted = ssd.is_deleted, -- Overwrites previous is_deleted
    job_status = ssd.job_status,
    pickup_details_id = ssd.pickup_details_id,
    delivery_details_id = ssd.delivery_details_id,
    goods_value = ssd.goods_value,
    goods_value_currency = ssd.goods_value_currency,
    insurance_value = ssd.insurance_value,
    insurance_value_currency = ssd.insurance_value_currency,
    shipment_created_on = ssd.shipment_created_on,
    entry_ref_no = ssd.entry_ref_no,
    flight_status = ssd.flight_status,
    contains_hazardous = ssd.contains_hazardous,
    fmc_tlc_id = ssd.fmc_tlc_id,
    commodity = ssd.commodity,
    order_number = ssd.order_number,
    order_management_id = ssd.order_management_id,
    order_management_number = ssd.order_management_number,
    customer_category = ssd.customer_category,
    contract_id = ssd.contract_id,
    contract_type = ssd.contract_type,
    client_country = ssd.client_country,
    consignor_country = ssd.consignor_country,
    consignee_country = ssd.consignee_country,
    notify_party_country = ssd.notify_party_country,
    sales_branch = ssd.sales_branch,
    source_guid = ssd.source_guid,
    consignee_dps_address_id = ssd.consignee_dps_address_id,
    client_dps_address_id = ssd.client_dps_address_id,
    consignor_dps_address_id = ssd.consignor_dps_address_id,
    notify_party_dps_address_id = ssd.notify_party_dps_address_id,
    primary_sales_agent_email = ssd.primary_sales_agent_email,
    secondary_sales_agent_email = ssd.secondary_sales_agent_email,
    booking_created_date = ssd.booking_created_date,
    security_status = ssd.security_status,
    current_party_for_quote = ssd.current_party_for_quote,
    cloned_guid = ssd.cloned_guid,
    entity_transfer = ssd.entity_transfer,
    destination_sales_branch = ssd.destination_sales_branch,
    destination_primary_sales_agent_email = ssd.destination_primary_sales_agent_email,
    destination_secondary_sales_agent_email = ssd.destination_secondary_sales_agent_email,
    destination_current_party_for_quote = ssd.destination_current_party_for_quote,
    destination_contract_id = ssd.destination_contract_id,
    destination_contract_type = ssd.destination_contract_type,
    shipment_gate_in_date = ssd.shipment_gate_in_date,
    date_type = ssd.date_type,
    shipment_pack_status = ssd.shipment_pack_status,
    file_status = ssd.file_status,
    cargo_ready_date = ssd.cargo_ready_date,
    cargo_delivery_date = ssd.cargo_delivery_date,
    parent_contract_id = ssd.parent_contract_id,
    is_receiving_branch_added = ssd.is_receiving_branch_added,
    department = ssd.department,
    ocean_dg_status = ssd.ocean_dg_status,
    sync_routing_from_consolidation = ssd.sync_routing_from_consolidation,
    is_network_file = ssd.is_network_file,
    is_receiving_branch_manually = ssd.is_receiving_branch_manually,
    triangulation_partner = ssd.triangulation_partner,
    is_et_transferred = ssd.is_et_transferred,
    is_transferred_to_receiving_branch = ssd.is_transferred_to_receiving_branch,
    triangulation_partner_single = ssd.triangulation_partner_single,
    transfer_status = ssd.transfer_status,
    b2b = ssd.b2b,
    is_co_load_enabled = ssd.is_co_load_enabled,
    co_load_carrier_name = ssd.co_load_carrier_name,
    co_load_bl_number = ssd.co_load_bl_number,
    issuing_carrier_name = ssd.issuing_carrier_name,
    ocean_bl_number = ssd.ocean_bl_number,
    customer_booking_guid = ssd.customer_booking_guid,
    is_frob = ssd.is_frob,
    origin_branch = ssd.origin_branch,
    incoterms_location = ssd.incoterms_location,
    controlled_reference_number = ssd.controlled_reference_number,
    partner = ssd.partner,
    co_load_bkg_number = ssd.co_load_bkg_number,
    booking_agent = ssd.booking_agent,
    pickup_at_origin_type = ssd.pickup_at_origin_type,
    delivery_at_destination_type = ssd.delivery_at_destination_type,
    brokerage_at_origin_type = ssd.brokerage_at_origin_type,
    brokerage_at_destination_type = ssd.brokerage_at_destination_type,
    pickup_at_origin = ssd.pickup_at_origin,
    delivery_at_destination = ssd.delivery_at_destination,
    brokerage_at_origin = ssd.brokerage_at_origin,
    brokerage_at_destination = ssd.brokerage_at_destination,
    brokerage_at_origin_date = ssd.brokerage_at_origin_date,
    brokerage_at_destination_date = ssd.brokerage_at_destination_date,
    terminal_cut_off = ssd.terminal_cut_off,
    verified_gross_mass_cut_off = ssd.verified_gross_mass_cut_off,
    shipping_instruction_cutoff = ssd.shipping_instruction_cutoff,
    dg_cut_off = ssd.dg_cut_off,
    reefer_cut_off = ssd.reefer_cut_off,
    earliest_empty_equipment_pickup = ssd.earliest_empty_equipment_pickup,
    latest_full_equipment_delivered_to_carrier = ssd.latest_full_equipment_delivered_to_carrier,
    earliest_drop_off_full_equipment_to_carrier = ssd.earliest_drop_off_full_equipment_to_carrier,
    latest_arrival_time = ssd.latest_arrival_time,
    is_reefer = ssd.is_reefer,
    cargo_readiness_date = ssd.cargo_readiness_date,
    container_assigned_to_shipment_cargo = ssd.container_assigned_to_shipment_cargo,
    controlled = ssd.controlled,
    is_borrowed = ssd.is_borrowed,
    borrowed_from = ssd.borrowed_from,
    slac = ssd.slac,
    transport_info_status = ssd.transport_info_status,
    dg_packs_count = ssd.dg_packs_count,
    dg_packs_unit = ssd.dg_packs_unit,
    is_migrated_to_v3 = ssd.is_migrated_to_v3
FROM __SCHEMA__.shipment_details AS ssd
WHERE psd.id = ssd.id AND ssd.tenant_id = __TENANT_ID__;






-- Table: public.allocations
UPDATE public.allocations SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.allocations AS pa
SET
    created_at = sa.created_at,
    created_by = sa.created_by,
    guid = sa.guid,
    updated_at = sa.updated_at,
    updated_by = sa.updated_by,
    tenant_id = sa.tenant_id,
    chargeable = sa.chargeable,
    chargeable_unit = sa.chargeable_unit,
    cutoff_date = sa.cutoff_date,
    hazardous = sa.hazardous,
    is_temperature_controlled = sa.is_temperature_controlled,
    max_temp = sa.max_temp,
    max_temp_unit = sa.max_temp_unit,
    min_temp = sa.min_temp,
    min_temp_unit = sa.min_temp_unit,
    shipments_count = sa.shipments_count,
    volume = sa.volume,
    volume_unit = sa.volume_unit,
    weight = sa.weight,
    weight_unit = sa.weight_unit,
    is_deleted = sa.is_deleted, -- Overwrites previous is_deleted
    weight_volume = sa.weight_volume,
    weight_volume_unit = sa.weight_volume_unit,
    packs = sa.packs,
    packs_type = sa.packs_type,
    container_count = sa.container_count,
    teu_count = sa.teu_count,
    dg_packs = sa.dg_packs,
    dg_packs_type = sa.dg_packs_type,
    dg_container_count = sa.dg_container_count
FROM __SCHEMA__.allocations AS sa
WHERE pa.id = sa.id AND sa.tenant_id = __TENANT_ID__;





-- Table: public.arrival_departure_details
UPDATE public.arrival_departure_details SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.arrival_departure_details AS pad
SET
    created_at = sad.created_at,
    created_by = sad.created_by,
    guid = sad.guid,
    updated_at = sad.updated_at,
    updated_by = sad.updated_by,
    tenant_id = sad.tenant_id,
    is_deleted = sad.is_deleted, -- Overwrites previous is_deleted
    container_yard_id = sad.container_yard_id,
    transport_port_id = sad.transport_port_id,
    cto_id = sad.cto_id,
    cfs_id = sad.cfs_id,
    first_foreign_port_id = sad.first_foreign_port_id,
    last_foreign_port_id = sad.last_foreign_port_id,
    first_foreign_port = sad.first_foreign_port,
    last_foreign_port = sad.last_foreign_port,
    "type" = sad."type",
    first_foreign_port_arrival_date = sad.first_foreign_port_arrival_date,
    last_foreign_port_departure_date = sad.last_foreign_port_departure_date
FROM __SCHEMA__.arrival_departure_details AS sad
WHERE pad.id = sad.id AND sad.tenant_id = __TENANT_ID__;




-- Table: public.containers
UPDATE public.containers SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.containers AS pc
SET
    created_at = sc.created_at,
    created_by = sc.created_by,
    guid = sc.guid,
    updated_at = sc.updated_at,
    updated_by = sc.updated_by,
    tenant_id = sc.tenant_id,
    achieved_volume = sc.achieved_volume,
    achieved_volume_unit = sc.achieved_volume_unit,
    achieved_weight = sc.achieved_weight,
    achieved_weight_unit = sc.achieved_weight_unit,
    allocated_volume = sc.allocated_volume,
    allocated_volume_unit_id = sc.allocated_volume_unit_id,
    allocated_weight = sc.allocated_weight,
    allocated_weight_unit = sc.allocated_weight_unit,
    allocation_date = sc.allocation_date,
    carrier_seal_number = sc.carrier_seal_number,
    chargeable = sc.chargeable,
    chargeable_unit = sc.chargeable_unit,
    commodity_code = sc.commodity_code,
    consolidation_id = sc.consolidation_id,
    container_code = sc.container_code,
    container_comments = sc.container_comments,
    container_count = sc.container_count,
    container_number = sc.container_number,
    container_stuffing_location = sc.container_stuffing_location,
    customs_release_code = sc.customs_release_code,
    customs_seal_number = sc.customs_seal_number,
    description_of_goods = sc.description_of_goods,
    dg_class = sc.dg_class,
    extra_params = sc.extra_params,
    gross_volume = sc.gross_volume,
    gross_volume_unit = sc.gross_volume_unit,
    gross_weight = sc.gross_weight,
    gross_weight_unit = sc.gross_weight_unit,
    hazardous = sc.hazardous,
    hazardous_un = sc.hazardous_un,
    hbl_delivery_mode = sc.hbl_delivery_mode,
    hs_code = sc.hs_code,
    inner_package_measurement_unit = sc.inner_package_measurement_unit,
    inner_package_number = sc.inner_package_number,
    inner_package_type = sc.inner_package_type,
    is_empty = sc.is_empty,
    is_own_container = sc.is_own_container,
    is_reefer = sc.is_reefer,
    is_shipper_owned = sc.is_shipper_owned,
    is_temperature_maintained = sc.is_temperature_maintained,
    logging_id = sc.logging_id,
    marks_n_nums = sc.marks_n_nums,
    max_temp = sc.max_temp,
    max_temp_unit = sc.max_temp_unit,
    measurement = sc.measurement,
    measurement_unit = sc.measurement_unit,
    min_temp = sc.min_temp,
    min_temp_unit_id = sc.min_temp_unit_id,
    net_weight = sc.net_weight,
    net_weight_unit = sc.net_weight_unit,
    no_of_packages = sc.no_of_packages,
    package_breadth = sc.package_breadth,
    package_height = sc.package_height,
    package_length = sc.package_length,
    packs = sc.packs,
    packs_type = sc.packs_type,
    pacr_number = sc.pacr_number,
    remarks = sc.remarks,
    seal_number = sc.seal_number,
    serial_number = sc.serial_number,
    shipper_seal_number = sc.shipper_seal_number,
    status = sc.status,
    tare_weight = sc.tare_weight,
    tare_weight_unit = sc.tare_weight_unit,
    terminal_operator_seal_number = sc.terminal_operator_seal_number,
    transport_mode = sc.transport_mode,
    veterinary_seal_number = sc.veterinary_seal_number,
    volume_utilization = sc.volume_utilization,
    weight_utilization = sc.weight_utilization,
    delivery_address_id = sc.delivery_address_id,
    pickup_address_id = sc.pickup_address_id,
    is_deleted = sc.is_deleted, -- Overwrites previous is_deleted
    booking_id = sc.booking_id,
    commodity_group = sc.commodity_group,
    is_contract_enforced = sc.is_contract_enforced,
    contract_enforced_quantity_limit = sc.contract_enforced_quantity_limit,
    own_type = sc.own_type,
    handling_info = sc.handling_info,
    is_part = sc.is_part,
    is_attached = sc.is_attached,
    invoice_number = sc.invoice_number,
    invoice_value = sc.invoice_value,
    invoice_currency = sc.invoice_currency,
    un_number = sc.un_number,
    proper_shipping_name = sc.proper_shipping_name,
    packing_group = sc.packing_group,
    minimum_flash_point = sc.minimum_flash_point,
    minimum_flash_point_unit = sc.minimum_flash_point_unit,
    marine_pollutant = sc.marine_pollutant,
    pra_status = sc.pra_status,
    humidity = sc.humidity,
    vents = sc.vents,
    teu = sc.teu
FROM __SCHEMA__.containers AS sc
WHERE pc.id = sc.id AND sc.tenant_id = __TENANT_ID__;




-- Table: public.file_repo
UPDATE public.file_repo SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.file_repo AS pfr
SET
    created_at = sfr.created_at,
    created_by = sfr.created_by,
    guid = sfr.guid,
    updated_at = sfr.updated_at,
    updated_by = sfr.updated_by,
    tenant_id = sfr.tenant_id,
    client_enabled = sfr.client_enabled,
    doc_type = sfr.doc_type,
    entity_id = sfr.entity_id,
    entity_type = sfr.entity_type,
    event_code = sfr.event_code,
    file_name = sfr.file_name,
    is_posted = sfr.is_posted,
    "path" = sfr."path",
    is_deleted = sfr.is_deleted -- Overwrites previous is_deleted
FROM __SCHEMA__.file_repo AS sfr
WHERE pfr.id = sfr.id AND sfr.tenant_id = __TENANT_ID__;




-- Table: public.booking_charges
UPDATE public.booking_charges SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.booking_charges AS pbc
SET
    created_at = sbc.created_at,
    created_by = sbc.created_by,
    guid = sbc.guid,
    booking_id = sbc.booking_id,
    is_deleted = sbc.is_deleted, -- Overwrites previous is_deleted
    updated_at = sbc.updated_at,
    updated_by = sbc.updated_by,
    tenant_id = sbc.tenant_id,
    seq_no = sbc.seq_no,
    charges_type = sbc.charges_type,
    details = sbc.details,
    charges_code_alt = sbc.charges_code_alt,
    hsn_master = sbc.hsn_master,
    measurement_basis = sbc.measurement_basis,
    measurement_container_type = sbc.measurement_container_type,
    total_unit_count = sbc.total_unit_count,
    measurement_unit = sbc.measurement_unit,
    cost_currency_exchange_update = sbc.cost_currency_exchange_update,
    reciprocal_currency_cost = sbc.reciprocal_currency_cost,
    estimated_cost = sbc.estimated_cost,
    local_cost_amount = sbc.local_cost_amount,
    overseas_cost_amount = sbc.overseas_cost_amount,
    overseas_cost_currency = sbc.overseas_cost_currency,
    current_cost_rate = sbc.current_cost_rate,
    cost_rate_currency = sbc.cost_rate_currency,
    cost_exchange = sbc.cost_exchange,
    cost_account = sbc.cost_account,
    cost_comments = sbc.cost_comments,
    creditor_id = sbc.creditor_id,
    cost_tax_id = sbc.cost_tax_id,
    cost_tax_date = sbc.cost_tax_date,
    cost_local_tax = sbc.cost_local_tax,
    cost_tax_percentage = sbc.cost_tax_percentage,
    cost_overseas_tax = sbc.cost_overseas_tax,
    cost_no_gst = sbc.cost_no_gst,
    cost_tax_type1 = sbc.cost_tax_type1,
    cost_tax_type2 = sbc.cost_tax_type2,
    cost_tax_type3 = sbc.cost_tax_type3,
    cost_tax_type4 = sbc.cost_tax_type4,
    cost_line_total = sbc.cost_line_total,
    sell_currency_exchange_update = sbc.sell_currency_exchange_update,
    reciprocal_currency_revenue = sbc.reciprocal_currency_revenue,
    estimated_revenue = sbc.estimated_revenue,
    local_sell_amount = sbc.local_sell_amount,
    overseas_sell_amount = sbc.overseas_sell_amount,
    overseas_sell_currency = sbc.overseas_sell_currency,
    current_sell_rate = sbc.current_sell_rate,
    sell_rate_currency = sbc.sell_rate_currency,
    sell_exchange = sbc.sell_exchange,
    revenue_account = sbc.revenue_account,
    revenue_comments = sbc.revenue_comments,
    debtor_id = sbc.debtor_id,
    revenue_tax_id = sbc.revenue_tax_id,
    revenue_tax_date = sbc.revenue_tax_date,
    local_tax = sbc.local_tax,
    tax_percentage = sbc.tax_percentage,
    overseas_tax = sbc.overseas_tax,
    no_gst = sbc.no_gst,
    tax_type_1 = sbc.tax_type_1,
    tax_type_2 = sbc.tax_type_2,
    tax_type_3 = sbc.tax_type_3,
    tax_type_4 = sbc.tax_type_4,
    revenue_line_total = sbc.revenue_line_total,
    local_cost_currency = sbc.local_cost_currency,
    local_sell_currency = sbc.local_sell_currency,
    internal_remarks = sbc.internal_remarks,
    external_remarks = sbc.external_remarks
FROM __SCHEMA__.booking_charges AS sbc
WHERE pbc.id = sbc.id AND sbc.tenant_id = __TENANT_ID__;





-- Table: public.container_charges_mapping
UPDATE public.container_charges_mapping
SET is_deleted = TRUE
WHERE container_id IN (
    SELECT id FROM public.containers WHERE tenant_id = __TENANT_ID__
);
UPDATE public.container_charges_mapping AS pccm
SET
    guid = sccm.guid,
    container_id = sccm.container_id, -- Added
    charge_id = sccm.charge_id,       -- Added
    created_at = sccm.created_at,
    updated_at = sccm.updated_at,
    created_by = sccm.created_by,
    updated_by = sccm.updated_by,
    is_deleted = sccm.is_deleted
FROM __SCHEMA__.container_charges_mapping AS sccm
WHERE pccm.id = sccm.id AND sccm.container_id IN (SELECT id FROM __SCHEMA__.containers WHERE tenant_id = __TENANT_ID__);


-- Table: public.consolidation_details
UPDATE public.consolidation_details SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.consolidation_details AS pcd
SET
    created_at = scd.created_at,
    created_by = scd.created_by,
    guid = scd.guid,
    updated_at = scd.updated_at,
    updated_by = scd.updated_by,
    tenant_id = scd.tenant_id,
    additional_terms = scd.additional_terms,
    booking_cutoff = scd.booking_cutoff,
    do_issue_date = scd.do_issue_date,
    estimated_terminal_cutoff = scd.estimated_terminal_cutoff,
    hazardous_booking_cutoff = scd.hazardous_booking_cutoff,
    igm_file_date = scd.igm_file_date,
    igm_inward_date = scd.igm_inward_date,
    is_sending_agent_freetext_address = scd.is_sending_agent_freetext_address,
    mawb = scd.mawb,
    marks_n_nums = scd.marks_n_nums,
    reefer_cutoff = scd.reefer_cutoff,
    smtp_igm_date = scd.smtp_igm_date,
    smtp_igm_number = scd.smtp_igm_number,
    ship_instruction_cutoff = scd.ship_instruction_cutoff,
    source_tenant_id = scd.source_tenant_id,
    terminal_cutoff = scd.terminal_cutoff,
    verified_gross_mass_cutoff = scd.verified_gross_mass_cutoff,
    agent_reference = scd.agent_reference,
    awb_dims = scd.awb_dims,
    bol = scd.bol,
    bonded_warehouse_id = scd.bonded_warehouse_id,
    booking_type = scd.booking_type,
    cargo_closing_time = scd.cargo_closing_time,
    co_load_booking_reference = scd.co_load_booking_reference,
    co_load_mbl = scd.co_load_mbl,
    consolidation_number = scd.consolidation_number,
    consolidation_type = scd.consolidation_type,
    container_category = scd.container_category,
    "copy" = scd."copy",
    declaration_type = scd.declaration_type,
    delivery_mode = scd.delivery_mode,
    description = scd.description,
    docs_closing_time = scd.docs_closing_time,
    documentation_partner = scd.documentation_partner,
    edi_transaction_id = scd.edi_transaction_id,
    igm_file_no = scd.igm_file_no,
    intra_branch = scd.intra_branch,
    inward_date_and_time = scd.inward_date_and_time,
    is_cargo_only = scd.is_cargo_only,
    is_charter = scd.is_charter,
    is_domestic = scd.is_domestic,
    is_inland = scd.is_inland,
    is_linked = scd.is_linked,
    is_locked = scd.is_locked,
    is_receiving_agent_freetext_address = scd.is_receiving_agent_freetext_address,
    locked_by = scd.locked_by,
    manifest_print = scd.manifest_print,
    masterbill_issue_date = scd.masterbill_issue_date,
    mrn_number = scd.mrn_number,
    msn_number = scd.msn_number,
    original = scd.original,
    override = scd.override,
    package_type = scd.package_type,
    payment = scd.payment,
    print_other_docs = scd.print_other_docs,
    receiving_agent_freetext_address = scd.receiving_agent_freetext_address,
    receiving_branch = scd.receiving_branch,
    reference_number = scd.reference_number,
    release_type = scd.release_type,
    sending_agent_freetext_address = scd.sending_agent_freetext_address,
    service_level = scd.service_level,
    shipment_type = scd.shipment_type,
    special_instructions = scd.special_instructions,
    transport_mode = scd.transport_mode,
    volume_utilization = scd.volume_utilization,
    warehouse_id = scd.warehouse_id,
    weight_utilization = scd.weight_utilization,
    achieved_quantities_id = scd.achieved_quantities_id,
    allocations_id = scd.allocations_id,
    carrier_detail_id = scd.carrier_detail_id,
    is_deleted = scd.is_deleted, -- Overwrites previous is_deleted
    arrival_details_id = scd.arrival_details_id,
    departure_details_id = scd.departure_details_id,
    sending_agent_id = scd.sending_agent_id,
    receiving_agent_id = scd.receiving_agent_id,
    borrowed_from_id = scd.borrowed_from_id,
    creditor_id = scd.creditor_id,
    co_load_with_id = scd.co_load_with_id,
    do_place_of_issue = scd.do_place_of_issue,
    place_of_issue = scd.place_of_issue,
    carrier_booking_ref = scd.carrier_booking_ref,
    first_load = scd.first_load,
    last_discharge = scd.last_discharge,
    mode_of_booking = scd.mode_of_booking,
    auto_update_goods_desc = scd.auto_update_goods_desc,
    source_guid = scd.source_guid,
    latest_full_equ_delivered_to_carrier = scd.latest_full_equ_delivered_to_carrier,
    earliest_drop_off_full_equ_to_carrier = scd.earliest_drop_off_full_equ_to_carrier,
    earliest_empty_equ_pick_up = scd.earliest_empty_equ_pick_up,
    booking_id = scd.booking_id,
    booking_status = scd.booking_status,
    booking_number = scd.booking_number,
    efreight_status = scd.efreight_status,
    hazardous = scd.hazardous,
    emergency_contact_number = scd.emergency_contact_number,
    emergency_contact_number_code = scd.emergency_contact_number_code,
    exemption_codes = scd.exemption_codes,
    aom_free_text = scd.aom_free_text,
    security_status = scd.security_status,
    sci = scd.sci,
    cfs_cut_off_date = scd.cfs_cut_off_date,
    open_for_attachment = scd.open_for_attachment,
    open_for_interbranch_attachment = scd.open_for_interbranch_attachment,
    lat_date = scd.lat_date,
    department = scd.department,
    additional_security_information = scd.additional_security_information,
    is_network_file = scd.is_network_file,
    sending_agent_country = scd.sending_agent_country,
    receiving_agent_country = scd.receiving_agent_country,
    is_receiving_branch_manually = scd.is_receiving_branch_manually,
    triangulation_partner = scd.triangulation_partner,
    is_et_transferred = scd.is_et_transferred,
    is_transferred_to_receiving_branch = scd.is_transferred_to_receiving_branch,
    triangulation_partner_single = scd.triangulation_partner_single,
    transfer_status = scd.transfer_status,
    booking_agent_id = scd.booking_agent_id,
    origin_branch = scd.origin_branch,
    co_load_carrier_name = scd.co_load_carrier_name,
    partner = scd.partner,
    is_borrowed = scd.is_borrowed,
    borrowed_from_organization_id = scd.borrowed_from_organization_id,
    reefer = scd.reefer,
    incoterms = scd.incoterms,
    is_migrated_to_v3 = scd.is_migrated_to_v3
FROM __SCHEMA__.consolidation_details AS scd
WHERE pcd.id = scd.id AND scd.tenant_id = __TENANT_ID__;





-- Table: public.customer_booking
UPDATE public.customer_booking SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.customer_booking AS pcb
SET
    created_at = scb.created_at,
    created_by = scb.created_by,
    guid = scb.guid,
    is_deleted = scb.is_deleted, -- Overwrites previous is_deleted
    updated_at = scb.updated_at,
    updated_by = scb.updated_by,
    tenant_id = scb.tenant_id,
    "source" = scb."source",
    contract_status = scb.contract_status,
    booking_status = scb.booking_status,
    customer_id = scb.customer_id,
    is_customer_free_text = scb.is_customer_free_text,
    consignor_id = scb.consignor_id,
    is_consignor_free_text = scb.is_consignor_free_text,
    consignee_id = scb.consignee_id,
    is_consignee_free_text = scb.is_consignee_free_text,
    notify_party_id = scb.notify_party_id,
    is_notify_party_free_text = scb.is_notify_party_free_text,
    customer_email = scb.customer_email,
    booking_number = scb.booking_number,
    booking_date = scb.booking_date,
    inco_terms = scb.inco_terms,
    carrier_detail_id = scb.carrier_detail_id,
    transport_type = scb.transport_type,
    cargo_type = scb.cargo_type,
    direction = scb.direction,
    quantity = scb.quantity,
    quantity_unit = scb.quantity_unit,
    gross_weight = scb.gross_weight,
    gross_weight_unit = scb.gross_weight_unit,
    volume = scb.volume,
    volume_unit = scb.volume_unit,
    weight_volume = scb.weight_volume,
    weight_volume_unit = scb.weight_volume_unit,
    chargeable = scb.chargeable,
    chargeable_unit = scb.chargeable_unit,
    is_platform_booking_created = scb.is_platform_booking_created,
    contract_id = scb.contract_id,
    service_mode = scb.service_mode,
    business_code = scb.business_code,
    shipment_id = scb.shipment_id,
    auto_update_weight_volume = scb.auto_update_weight_volume,
    fmc_tlc_id = scb.fmc_tlc_id,
    is_package_manual = scb.is_package_manual,
    is_consignor_address_free_text = scb.is_consignor_address_free_text,
    is_consignee_address_free_text = scb.is_consignee_address_free_text,
    is_customer_address_free_text = scb.is_customer_address_free_text,
    is_notify_party_address_free_text = scb.is_notify_party_address_free_text,
    total_revenue = scb.total_revenue,
    shipment_entity_id = scb.shipment_entity_id,
    shipment_created_date = scb.shipment_created_date,
    shipment_guid = scb.shipment_guid,
    shipment_entity_id_v2 = scb.shipment_entity_id_v2,
    client_country = scb.client_country,
    consignor_country = scb.consignor_country,
    consignee_country = scb.consignee_country,
    notify_party_country = scb.notify_party_country,
    parent_contract_id = scb.parent_contract_id,
    sales_branch = scb.sales_branch,
    is_notify_consignee_equal = scb.is_notify_consignee_equal,
    primary_sales_agent_email = scb.primary_sales_agent_email,
    secondary_sales_agent_email = scb.secondary_sales_agent_email,
    is_bill_created = scb.is_bill_created,
    current_party_for_quote = scb.current_party_for_quote,
    source_guid = scb.source_guid,
    order_management_number = scb.order_management_number,
    order_management_id = scb.order_management_id,
    is_dg = scb.is_dg,
    rejection_remarks = scb.rejection_remarks,
    shipment_reference_number = scb.shipment_reference_number,
    payment_terms = scb.payment_terms,
    incoterms_location = scb.incoterms_location,
    controlled_reference_number = scb.controlled_reference_number,
    partner = scb.partner,
    booking_agent = scb.booking_agent,
    pickup_at_origin_type = scb.pickup_at_origin_type,
    delivery_at_destination_type = scb.delivery_at_destination_type,
    brokerage_at_origin_type = scb.brokerage_at_origin_type,
    brokerage_at_destination_type = scb.brokerage_at_destination_type,
    brokerage_at_origin = scb.brokerage_at_origin,
    brokerage_at_destination = scb.brokerage_at_destination,
    brokerage_at_origin_date = scb.brokerage_at_origin_date,
    brokerage_at_destination_date = scb.brokerage_at_destination_date,
    terminal_cut_off = scb.terminal_cut_off,
    verified_gross_mass_cut_off = scb.verified_gross_mass_cut_off,
    shipping_instruction_cutoff = scb.shipping_instruction_cutoff,
    dg_cut_off = scb.dg_cut_off,
    eta = scb.eta,
    etd = scb.etd,
    reefer_cut_off = scb.reefer_cut_off,
    earliest_empty_equipment_pickup = scb.earliest_empty_equipment_pickup,
    latest_full_equipment_delivered_to_carrier = scb.latest_full_equipment_delivered_to_carrier,
    earliest_drop_off_full_equipment_to_carrier = scb.earliest_drop_off_full_equipment_to_carrier,
    latest_arrival_time = scb.latest_arrival_time,
    is_reefer = scb.is_reefer,
    cargo_readiness_date = scb.cargo_readiness_date,
    controlled = scb.controlled,
    teu_count = scb.teu_count,
    containers = scb.containers,
    package_type = scb.package_type,
    packages = scb.packages,
    description = scb.description,
    marks_n_numbers = scb.marks_n_numbers,
    additional_terms = scb.additional_terms,
    partner_bkg_number = scb.partner_bkg_number,
    partner_bl_or_awb_number = scb.partner_bl_or_awb_number,
    carrier_bkg_number = scb.carrier_bkg_number,
    pickup_at_origin_date = scb.pickup_at_origin_date,
    delivery_at_destination_date = scb.delivery_at_destination_date,
    pickup_at_origin = scb.pickup_at_origin,
    delivery_at_destination = scb.delivery_at_destination,
    cargo_delivery_date = scb.cargo_delivery_date,
    co_load_carrier_name = scb.co_load_carrier_name,
    is_migrated_to_v3 = scb.is_migrated_to_v3
FROM __SCHEMA__.customer_booking AS scb
WHERE pcb.id = scb.id AND scb.tenant_id = __TENANT_ID__;






-- Table: public.network_transfer
UPDATE public.network_transfer SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.network_transfer AS pnt
SET
    created_at = snt.created_at,
    created_by = snt.created_by,
    guid = snt.guid,
    updated_at = snt.updated_at,
    updated_by = snt.updated_by,
    is_deleted = snt.is_deleted, -- Overwrites previous is_deleted
    tenant_id = snt.tenant_id,
    entity_type = snt.entity_type,
    entity_number = snt.entity_number,
    entity_id = snt.entity_id,
    created_entity_id = snt.created_entity_id,
    transport_mode = snt.transport_mode,
    source_branch_id = snt.source_branch_id,
    status = snt.status,
    job_type = snt.job_type,
    entity_payload = snt.entity_payload,
    is_inter_branch_entity = snt.is_inter_branch_entity,
    entity_guid = snt.entity_guid,
    is_hidden = snt.is_hidden,
    transferred_date = snt.transferred_date,
    "source" = snt."source",
    is_migrated_to_v3 = snt.is_migrated_to_v3
FROM __SCHEMA__.network_transfer AS snt
WHERE pnt.id = snt.id AND snt.tenant_id = __TENANT_ID__;




-- Table: public.carrier_details
UPDATE public.carrier_details SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.carrier_details AS pcd
SET
    created_at = scd.created_at,
    created_by = scd.created_by,
    guid = scd.guid,
    updated_at = scd.updated_at,
    updated_by = scd.updated_by,
    tenant_id = scd.tenant_id,
    aircraft_registration = scd.aircraft_registration,
    aircraft_type = scd.aircraft_type,
    ata = scd.ata,
    atd = scd.atd,
    destination = scd.destination,
    eta = scd.eta,
    etd = scd.etd,
    flight_number = scd.flight_number,
    journey_number = scd.journey_number,
    journey_ref_number = scd.journey_ref_number,
    origin = scd.origin,
    shipping_line = scd.shipping_line,
    truck_ref_number = scd.truck_ref_number,
    vessel = scd.vessel,
    voyage = scd.voyage,
    is_deleted = scd.is_deleted, -- Overwrites previous is_deleted
    origin_port = scd.origin_port,
    destination_port = scd.destination_port,
    vessel_berthing_date = scd.vessel_berthing_date,
    carrier_country = scd.carrier_country,
    min_transit_hours = scd.min_transit_hours,
    max_transit_hours = scd.max_transit_hours,
    carrier_added_from_npm = scd.carrier_added_from_npm,
    cfs = scd.cfs,
    origin_loc_code = scd.origin_loc_code,
    destination_loc_code = scd.destination_loc_code,
    origin_port_loc_code = scd.origin_port_loc_code,
    destination_port_loc_code = scd.destination_port_loc_code,
    is_carrier_changed = scd.is_carrier_changed,
    is_same_as_origin_port = scd.is_same_as_origin_port,
    is_same_as_destination_port = scd.is_same_as_destination_port,
    origin_country = scd.origin_country,
    destination_country = scd.destination_country,
    origin_port_country = scd.origin_port_country,
    destination_port_country = scd.destination_port_country,
    vehicle_number = scd.vehicle_number
FROM __SCHEMA__.carrier_details AS scd
WHERE pcd.id = scd.id AND scd.tenant_id = __TENANT_ID__;





-- Table: public.pickup_delivery_details
UPDATE public.pickup_delivery_details SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.pickup_delivery_details AS ppd
SET
    created_at = spd.created_at,
    created_by = spd.created_by,
    guid = spd.guid,
    updated_at = spd.updated_at,
    updated_by = spd.updated_by,
    tenant_id = spd.tenant_id,
    actual_pickup_or_delivery = spd.actual_pickup_or_delivery,
    estimated_pickup_or_delivery = spd.estimated_pickup_or_delivery,
    pickup_or_delivery = spd.pickup_or_delivery,
    port_transport_advised = spd.port_transport_advised,
    required_by = spd.required_by,
    shipment_id = spd.shipment_id,
    "type" = spd."type",
    agent_id = spd.agent_id,
    broker_id = spd.broker_id,
    destination_id = spd.destination_id,
    source_id = spd.source_id,
    transporter_id = spd.transporter_id,
    is_deleted = spd.is_deleted, -- Overwrites previous is_deleted
    drop_mode = spd.drop_mode,
    labour_charge = spd.labour_charge,
    labour_charge_unit = spd.labour_charge_unit,
    labour_duration = spd.labour_duration,
    shipper_ref = spd.shipper_ref,
    interim_receipt = spd.interim_receipt,
    fcl_available_date = spd.fcl_available_date,
    storage_date = spd.storage_date,
    truck_wait_time_charge = spd.truck_wait_time_charge,
    truck_wait_time_charge_unit = spd.truck_wait_time_charge_unit,
    truck_wait_duration = spd.truck_wait_duration,
    storage_charge = spd.storage_charge,
    storage_charge_unit = spd.storage_charge_unit,
    storage_charge_duration = spd.storage_charge_duration,
    ucr_reference = spd.ucr_reference,
    empty_truck_in_date = spd.empty_truck_in_date,
    loaded_truck_gate_out_date = spd.loaded_truck_gate_out_date,
    pickup_delivery_instruction = spd.pickup_delivery_instruction,
    pickup_delivery_gate_in = spd.pickup_delivery_gate_in,
    pickup_delivery_gate_out = spd.pickup_delivery_gate_out,
    remarks = spd.remarks,
    actual_delivery = spd.actual_delivery,
    estimated_delivery = spd.estimated_delivery,
    delivery_gate_in = spd.delivery_gate_in,
    delivery_gate_out = spd.delivery_gate_out,
    actual_pickup = spd.actual_pickup,
    estimated_pickup = spd.estimated_pickup,
    pickup_gate_out = spd.pickup_gate_out,
    pickup_gate_in = spd.pickup_gate_in,
    is_direct_delivery = spd.is_direct_delivery,
    ti_reference_number = spd.ti_reference_number
FROM __SCHEMA__.pickup_delivery_details AS spd
WHERE ppd.id = spd.id AND spd.tenant_id = __TENANT_ID__;





-- Table: public.parties
UPDATE public.parties SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.parties AS pp
SET
    created_at = sp.created_at,
    created_by = sp.created_by,
    guid = sp.guid,
    updated_at = sp.updated_at,
    updated_by = sp.updated_by,
    tenant_id = sp.tenant_id,
    address_code = sp.address_code,
    address_data = sp.address_data,
    entity_id = sp.entity_id,
    entity_type = sp.entity_type,
    org_code = sp.org_code,
    org_data = sp.org_data,
    party_type = sp.party_type,
    is_deleted = sp.is_deleted, -- Overwrites previous is_deleted
    is_address_free_text = sp.is_address_free_text,
    org_id = sp.org_id,
    address_id = sp.address_id,
    country_code = sp.country_code
FROM __SCHEMA__.parties AS sp
WHERE pp.id = sp.id AND sp.tenant_id = __TENANT_ID__;





-- Table: public.achieved_quantities
UPDATE public.achieved_quantities SET is_deleted = TRUE WHERE tenant_id = __TENANT_ID__;
UPDATE public.achieved_quantities AS paq
SET
    created_at = saq.created_at,
    created_by = saq.created_by,
    guid = saq.guid,
    updated_at = saq.updated_at,
    updated_by = saq.updated_by,
    tenant_id = saq.tenant_id,
    consolidated_volume = saq.consolidated_volume,
    consolidated_volume_unit = saq.consolidated_volume_unit,
    consolidated_weight = saq.consolidated_weight,
    consolidated_weight_unit = saq.consolidated_weight_unit,
    consolidation_charge_quantity = saq.consolidation_charge_quantity,
    consolidation_charge_quantity_unit = saq.consolidation_charge_quantity_unit,
    weight_volume = saq.weight_volume,
    weight_volume_unit = saq.weight_volume_unit,
    weight_utilization = saq.weight_utilization,
    volume_utilization = saq.volume_utilization,
    is_deleted = saq.is_deleted, -- Overwrites previous is_deleted
    packs = saq.packs,
    packs_type = saq.packs_type,
    container_count = saq.container_count,
    teu_count = saq.teu_count,
    dg_packs = saq.dg_packs,
    dg_packs_type = saq.dg_packs_type,
    dg_container_count = saq.dg_container_count,
    slac_count = saq.slac_count
FROM __SCHEMA__.achieved_quantities AS saq
WHERE paq.id = saq.id AND saq.tenant_id = __TENANT_ID__;





-- Table: public.shipment_order
UPDATE public.shipment_order
SET is_deleted = TRUE
WHERE shipment_id IN (
    SELECT id FROM public.shipment_details WHERE tenant_id = __TENANT_ID__
);
UPDATE public.shipment_order AS pso
SET
    id = sso.id,
    guid = sso.guid,
    order_guid = sso.order_guid,  -- Added
    order_number = sso.order_number,
    shipment_id = sso.shipment_id, -- Added
    created_at = sso.created_at,
    updated_at = sso.updated_at,
    created_by = sso.created_by,
    updated_by = sso.updated_by,
    is_deleted = sso.is_deleted
FROM __SCHEMA__.shipment_order AS sso
WHERE pso.shipment_id = sso.shipment_id AND pso.order_guid = sso.order_guid AND sso.shipment_id IN (SELECT id FROM __SCHEMA__.shipment_details WHERE tenant_id = __TENANT_ID__);

DELETE FROM public.screening_status_consol where  screening_status_consol.consolidation_details_id in (select id from public.consolidation_details where tenant_id = __TENANT_ID__);

DELETE FROM public.screening_status where  shipment_additional_details_id in (select id from public.shipment_additional_details where tenant_id = __TENANT_ID__);

DELETE FROM public.triangulation_partner_consolidation where consolidation_id in (select id from public.consolidation_details where tenant_id = __TENANT_ID__);

DELETE FROM public.triangulation_partner_shipment where shipment_id in (select id from public.shipment_details where tenant_id = __TENANT_ID__);


DELETE FROM public.dps_approval_detail WHERE dps_event_id IN (SELECT id FROM public.dps_event WHERE entity_id IN (SELECT guid::text FROM public.shipment_details WHERE tenant_id = __TENANT_ID__));


DELETE FROM public.dps_event_condition_message WHERE dps_event_id IN (SELECT id FROM public.dps_event WHERE entity_id IN (SELECT guid::text FROM public.shipment_details WHERE tenant_id = __TENANT_ID__));

DELETE FROM public.dps_event_implication WHERE dps_event_id IN (SELECT id FROM public.dps_event WHERE entity_id IN (SELECT guid::text FROM public.shipment_details WHERE tenant_id = __TENANT_ID__));

DELETE FROM public.dps_event_rule_matched_field WHERE dps_event_id IN (SELECT id FROM public.dps_event WHERE entity_id IN (SELECT guid::text FROM public.shipment_details WHERE tenant_id = __TENANT_ID__));

DELETE FROM public.dps_event WHERE entity_id IN (SELECT guid::text FROM public.shipment_details WHERE tenant_id = __TENANT_ID__);



INSERT INTO public.dps_event  SELECT * FROM __SCHEMA__.dps_event
WHERE entity_id IN (
    SELECT guid::text FROM __SCHEMA__.shipment_details WHERE tenant_id = __TENANT_ID__
);

INSERT INTO public.dps_approval_detail SELECT * FROM __SCHEMA__.dps_approval_detail  WHERE dps_event_id IN (
    SELECT id FROM __SCHEMA__.dps_event
    WHERE entity_id IN (
        SELECT guid::text FROM __SCHEMA__.shipment_details WHERE tenant_id = __TENANT_ID__
    )
);

INSERT INTO public.dps_event_condition_message SELECT * FROM __SCHEMA__.dps_event_condition_message
WHERE dps_event_id IN (
    SELECT id FROM __SCHEMA__.dps_event
    WHERE entity_id IN (
        SELECT guid::text FROM __SCHEMA__.shipment_details WHERE tenant_id = __TENANT_ID__
    )
);

INSERT INTO public.dps_event_implication
SELECT * FROM __SCHEMA__.dps_event_implication
WHERE dps_event_id IN (
    SELECT id FROM __SCHEMA__.dps_event
    WHERE entity_id IN (
        SELECT guid::text FROM __SCHEMA__.shipment_details WHERE tenant_id = __TENANT_ID__
    )
);

INSERT INTO public.dps_event_rule_matched_field
SELECT * FROM __SCHEMA__.dps_event_rule_matched_field
WHERE dps_event_id IN (
    SELECT id FROM __SCHEMA__.dps_event
    WHERE entity_id IN (
        SELECT guid::text FROM __SCHEMA__.shipment_details WHERE tenant_id = __TENANT_ID__
    )
);

INSERT INTO public.screening_status_consol select * from __SCHEMA__.screening_status_consol where consolidation_details_id in (select id from __SCHEMA__.consolidation_details where tenant_id = __TENANT_ID__);

INSERT INTO public.screening_status select * from __SCHEMA__.screening_status where shipment_additional_details_id in (select id from __SCHEMA__.shipment_additional_details where tenant_id = __TENANT_ID__);

INSERT INTO public.triangulation_partner_consolidation SELECT * FROM __SCHEMA__.triangulation_partner_consolidation WHERE consolidation_id in (select id from __SCHEMA__.consolidation_details where tenant_id = __TENANT_ID__);

INSERT INTO public.triangulation_partner_shipment SELECT * FROM __SCHEMA__.triangulation_partner_shipment WHERE shipment_id in (select id from __SCHEMA__.shipment_details where tenant_id = __TENANT_ID__);
