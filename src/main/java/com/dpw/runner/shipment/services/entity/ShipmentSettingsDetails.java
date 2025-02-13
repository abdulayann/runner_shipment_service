package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.enums.GenerationType;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.MasterData;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.BatchSize;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Setter
@Getter
@Builder
@Table(name = "shipment_setting")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@SQLDelete(sql = "UPDATE shipment_setting SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class ShipmentSettingsDetails extends MultiTenancy {
    private static final long serialVersionUID = 1L;

    @Column(name = "house_bill_number_lock")
    private Boolean houseBillNumberLock;

    @Column(name = "restrict_hbl_gen")
    private Boolean restrictHblGen;

    @Column(name = "print_phone_number")
    private Boolean printPhoneNumber;

    @Column(name = "house_bill_prefix")
    private String housebillPrefix;

    @Column(name = "house_bill_number_generation")
    private String housebillNumberGeneration;

    @Column(name = "footer_columns")
    private Integer footerColumns;

    @Column(name = "is_auto_populate_ship_type")
    private Boolean isAutoPopulateShipType;

    @Column(name = "partial_cloning_enabled")
    private Boolean partialCloningEnabled;

    @Column(name = "ship_consolidation_container_enabled")
    private Boolean shipConsolidationContainerEnabled;

    @Column(name = "is_shipment_level_container")
    private Boolean isShipmentLevelContainer;

    @Column(name = "auto_attach_consolidation")
    private Boolean autoAttachConsolidation;

    @Column(name = "multiple_shipment_enabled")
    private Boolean multipleShipmentEnabled;

    @Column(name = "enable_route_master")
    private Boolean enableRouteMaster;

    @Column(name = "ar_ap_flag")
    private Boolean arApFlag;

    @Column(name = "shipment_ti_Carges_linkage")
    private Boolean shipmentTiCargesLinkage;

    @Column(name = "cooldown_time")
    private Integer cooldownTime;

    @Column(name = "advance_period")
    private Integer advancePeriod;

    @Column(name = "auto_event_create")
    private Boolean autoEventCreate;

    @Column(name = "shipment_lite")
    private Boolean shipmentLite;

    @Column(name = "billing_lite")
    private Boolean billingLite;

    @Column(name = "restricted_locations_enabled")
    private Boolean restrictedLocationsEnabled;

    @Column(name = "is_atd_ata_auto_populate_enabled")
    private Boolean isAtdAtaAutoPopulateEnabled;

    @Column(name = "restricted_locations")
    @ElementCollection(targetClass = String.class, fetch = FetchType.LAZY)
    @CollectionTable(name = "shipment_settings_details_restricted_locations", joinColumns = @JoinColumn(name = "shipment_settings_details_id"))
//    @OneToMany(fetch = FetchType.EAGER)
    @BatchSize(size = 50)
    private List<String> restrictedLocations;

    @Column(name = "shipment_console_import_approver_role")
    private String shipmentConsoleImportApproverRole;

    @Column(name = "regulated_agent")
    private Boolean regulatedAgent;

    @Column(name = "ra_number")
    private String raNumber;

    @Column(name = "ra_expiry")
    private LocalDateTime raExpiry;

    @Enumerated(EnumType.ORDINAL)
    @Column(name = "shipment_id_generation_type")
    private GenerationType shipmentIdGenerationType;

    @Column(name = "shipment_id_generation_prefix")
    private String shipmentIdGenerationPrefix;

    @Column(name = "shipment_id_generation_counter")
    private Integer shipmentIdGenerationCounter;

    @Column(name = "low_margin_approval")
    private Boolean lowMarginApproval;

    @Where(clause = "is_front_print = true")
    @OneToMany(fetch = FetchType.LAZY, mappedBy = "shipmentSettingsId")
    @BatchSize(size = 50)
    private List<HblTermsConditionTemplate> hblTermsConditionTemplate;

    @Where(clause = "is_front_print = false")
    @OneToMany(fetch = FetchType.LAZY, mappedBy = "shipmentSettingsId")
    @BatchSize(size = 50)
    private List<HblTermsConditionTemplate> hblHawbBackPrintTemplate;

    // CommonSettings

    @Column(name = "inline_enabled")
    private Boolean inlineEnabled;

    @Column(name = "carrier_mandatory")
    private Boolean carrierMandatory;

    @Column(name = "weight_volume_decimal_place")
    private Integer weightVolumeDecimalPlace;

    @Column(name = "dpw_date_format")
    private String dpwDateFormat;

    @Column(name = "weight_chargeable_unit")
    @MasterData(type = MasterDataType.WEIGHT_UNIT)
    private String weightChargeableUnit;

    @MasterData(type = MasterDataType.VOLUME_UNIT)
    @Column(name = "volume_chargeable_unit")
    private String volumeChargeableUnit;

    @Column(name = "measurement_chargeable_unit")
    @MasterData(type = MasterDataType.DIMENSION_UNIT)
    private String measurementChargeableUnit;

    @MasterData(type = MasterDataType.TEMPERATURE_UNIT)
    @Column(name = "temperature_unit")
    private String temperatureUnit;

    @MasterData(type = MasterDataType.TRANSPORT_MODE)
    @Column(name = "default_transport_mode")
    private String defaultTransportMode;

    @Column(name = "default_container_type")
    private String defaultContainerType;

    @MasterData(type = MasterDataType.SHIPMENT_TYPE)
    @Column(name = "default_shipment_type")
    private String defaultShipmentType;

    // Email Settings

    @Column(name = "email_host")
    private String emailHost;

    @Column(name = "email_port")
    private Long emailPort;

    @Column(name = "email_use_ssl")
    private Boolean emailUseSsl;

    @Column(name = "email_from")
    private String emailFrom;

    @Column(name = "email_user_name")
    private String emailUserName;

    @Column(name = "email_password")
    private String emailPassword;

    @Column(name = "email_isf_asm_response")
    private Boolean emailIsfAsmResponse;

    @Column(name = "default_failure_email_ids")
    private String defaultFailureEmailIds;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "shipmentSettingsId")
    @BatchSize(size = 50)
    private List<EmailTemplates> emailTemplates;

    // Quote Settings

    @Column(name = "shipment_lock")
    private Boolean shipmentLock;

    // Organization Settings

    @Column(name = "org_prefix")
    private String orgPrefix;

    @Column(name = "org_code_lock")
    private Boolean orgCodeLock;

    @Column(name = "country_prefix")
    private Boolean countryPrefix;

    @Enumerated(EnumType.ORDINAL)
    @Column(name = "org_number_generation")
    private GenerationType orgNumberGeneration;

    @Column(name = "un_loco_code_enabled")
    private Boolean unLocoCodeEnabled;

    @Column(name = "enable_org_address_update")
    private Boolean enableOrgAddressUpdate;

    @Column(name = "allow_free_text_address")
    private Boolean allowFreeTextAddress;

    // Consolidation Settings

    @Column(name = "is_consolidator")
    private Boolean isConsolidator;

    @Column(name = "merge_containers")
    private Boolean mergeContainers;

    @Column(name = "bol_number_lock")
    private Boolean bolNumberLock;

    @Column(name = "is_show_mbl")
    private Boolean isShowMBL;

    @Column(name = "consolidation_lite")
    private Boolean consolidationLite;

    @Column(name = "container_event_bulk_upload")
    private Boolean containerEventBulkUpload;

    @Column(name = "bol_number_prefix")
    private String bolNumberPrefix;

    @Enumerated(EnumType.ORDINAL)
    @Column(name = "bol_number_generation")
    private GenerationType bolNumberGeneration;

    // Document Settings

    @Column(name = "is_mandate_doc_type")
    private Boolean isMandateDocType;

    @Column(name = "house_main_page")
    private String houseMainPage;

    @Column(name = "hbl_footer")
    private String hblFooter;

    @Column(name = "print_after_each_page")
    private Boolean printAfterEachPage;

    @Column(name = "sea_way_main_page")
    private String seawayMainPage;

    @Column(name = "ship_truck_way_bill_main_page")
    private String shipTruckWayBillMainPage;

    @Column(name = "cons_truck_way_bill_main_page")
    private String consTruckWayBillMainPage;

    @Column(name = "ship_truck_driver_proof")
    private String shipTruckDriverProof;

    @Column(name = "cons_truck_driver_proof")
    private String consTruckDriverProof;

    @Column(name = "commercial_inv_main_page")
    private String commercialInvMainPage;

    @Column(name = "packing_list_main_page")
    private String packingListMainPage;

    @Column(name = "customs_ins_main_page")
    private String customsInsMainPage;

    @Column(name = "airway_main_page")
    private String airwayMainPage;

    @Column(name = "can_main_page")
    private String canMainPage;

    @Column(name = "can_back_print")
    private String canBackPrint;

    @Column(name = "arrival_notice")
    private String arrivalNotice;

    @Column(name = "freight_certification")
    private String freightCertification;

    @Column(name = "pre_alert_doc")
    private String preAlertDoc;

    @Column(name = "manifest_print")
    private String manifestPrint;

    @Column(name = "container_manifest_print")
    private String containerManifestPrint;

    @Column(name = "sea_import_shipment_manifest")
    private String seaImportShipmentManifest;

    @Column(name = "sea_export_shipment_manifest")
    private String seaExportShipmentManifest;

    @Column(name = "sea_import_console_manifest")
    private String seaImportConsoleManifest;

    @Column(name = "sea_export_console_manifest")
    private String seaExportConsoleManifest;

    @Column(name = "air_import_shipment_manifest")
    private String airImportShipmentManifest;

    @Column(name = "air_export_shipment_manifest")
    private String airExportShipmentManifest;

    @Column(name = "air_import_console_manifest")
    private String airImportConsoleManifest;

    @Column(name = "air_export_console_manifest")
    private String airExportConsoleManifest;

    @Column(name = "proof_of_delivery")
    private String proofOfDelivery;

    @Column(name = "pickup_order")
    private String pickupOrder;

    @Column(name = "delivery_order")
    private String deliveryOrder;

    @Column(name = "booking_confirmation")
    private String bookingConfirmation;

    @Column(name = "costal_document")
    private String costalDocument;

    @Column(name = "shipping_instruction")
    private String shippingInstruction;

    @Column(name = "consolidated_packing_list")
    private String consolidatedPackingList;

    @Column(name = "awb_lable")
    private String awbLable;

    @Column(name = "cargo_manifest")
    private String cargoManifest;

    @Column(name = "packing_list_main_page_air")
    private String packingListMainPageAir;

    @Column(name = "freight_certification_air")
    private String freightCertificationAir;

    @Column(name = "pre_alert_air")
    private String preAlertAir;

    @Column(name = "booking_confirmation_air")
    private String bookingConfirmationAir;

    @Column(name = "pickup_order_air")
    private String pickupOrderAir;

    @Column(name = "delivery_order_air")
    private String deliveryOrderAir;

    @Column(name = "can_main_page_air")
    private String canMainPageAir;

    @Column(name = "can_back_print_air")
    private String canBackPrintAir;

    @Column(name = "customs_ins_main_page_air")
    private String customsInsMainPageAir;

    @Column(name = "commercial_inv_main_page_air")
    private String commercialInvMainPageAir;

    @Column(name = "arrival_notice_air")
    private String arrivalNoticeAir;

    @Column(name = "hawb")
    private String hawb;

    @Column(name = "mawb")
    private String mawb;

    @Column(name = "awb_neutral")
    private String awbNeutral;

    @Column(name = "csr")
    private String csr;

    @Column(name = "shipping_request_main_page")
    private String shippingRequestMainPage;

    @Column(name = "shipping_request_air")
    private String shippingRequestAir;

    @Column(name = "sea_shipping_instruction_main_page")
    private String seaShippingInstructionMainPage;

    @Column(name = "default_email_template")
    private String defaultEmailTemplate;

    // BL Lock Settings

    @Column(name = "decimal_places")
    private Integer decimalPlaces;

    @Column(name = "hbl_approval_role")
    private Integer hblApprovalRole;

    @Column(name = "cargo_finance_booking")
    private Boolean cargoFinanceBooking;

    @Column(name = "unico_bl_object")
    private Boolean unicoBlObject;

    @Column(name = "restrict_bl_edit")
    private Boolean restrictBLEdit;

    @Column(name = "restrict_awb_edit")
    private Boolean restrictAWBEdit;

    @Column(name = "auto_update_shipment_awb")
    private Boolean autoUpdateShipmentAWB;

    @Column(name = "auto_update_shipment_bl")
    private Boolean autoUpdateShipmentBL;

    @OneToOne(targetEntity = HblLockSettings.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "hbl_lock_settings_id", referencedColumnName = "id")
    private HblLockSettings hblLockSettings;

    // Sequence Configuration

    @Column(name = "customised_sequence")
    private Boolean customisedSequence;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "shipmentSettingsId")
    @BatchSize(size = 50)
    private List<TenantProducts> tenantProducts;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "shipmentSettingsId")
    @BatchSize(size = 50)
    private List<ProductSequenceConfig> productSequenceConfig;

    //AWB Lock Settings

    @OneToOne(targetEntity = HawbLockSettings.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "hawb_lock_settings_id", referencedColumnName = "id")
    private HawbLockSettings hawbLockSettings;

    @OneToOne(targetEntity = MawbLockSettings.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "mawb_lock_settings_id", referencedColumnName = "id")
    private MawbLockSettings mawbLockSettings;

    @Column(name = "hbl_multiple_original_approval")
    private Integer hblMultipleOriginalApproval;

    @Column(name = "hbl_approval_flow")
    private Boolean hblApprovalFlow;

    @Column(name = "restrict_bl_release")
    private Boolean restrictBlRelease;

    @Column(name = "restrict_bl_approval_role")
    private Integer restrictBlApprovalRole;

    @Column(name = "e_manifest")
    private Boolean eManifest;

    @Column(name = "isf_file_main_page")
    private String isfFileMainPage;

    @Column(name = "weight_decimal_place")
    private Integer weightDecimalPlace;

    @Column(name = "volume_decimal_place")
    private Integer volumeDecimalPlace;

    @Column(name = "cancelled_bl_suffix")
    private String cancelledBLSuffix;

    @Column(name = "transport_order_road")
    private String transportOrderRoad;

    @Column(name = "disable_bl_parties_name")
    private Boolean disableBlPartiesName;

    @Column(name = "air_dg_flag")
    private Boolean airDGFlag;

    @Column(name = "iata_tact_flag")
    private Boolean iataTactFlag;

    @Column(name = "enable_lcl_consolidation")
    private Boolean enableLclConsolidation;

    @Column(name = "booking_order")
    private String bookingOrder;

    @Column(name = "booking_order_mbl")
    private String bookingOrderForMbl;

    @Column(name = "booking_order_air")
    private String bookingOrderAir;

    @Column(name = "booking_order_air_mawb")
    private String bookingOrderAirForMawb;

    @Column(name = "transport_instruction_pickup_order")
    private String transportInstructionPickupOrder;

    @Column(name = "transport_instruction_delivery_order")
    private String transportInstructionDeliveryOrder;
    @Column(name = "enable_party_check_for_consolidation")
    private Boolean enablePartyCheckForConsolidation;

    @Column(name = "csd")
    private String csd;

    @Column(name = "hide_manifest")
    private Boolean hideManifest = true;

    @Column(name = "is_entity_transfer_prerequisite_enabled_date")
    private LocalDateTime isEntityTransferPrerequisiteEnabledDate = null;

    @Column(name = "is_entity_transfer_prerequisite_enabled")
    private Boolean isEntityTransferPrerequisiteEnabled = false;

    @Column(name = "is_network_transfer_entity_enabled")
    private Boolean isNetworkTransferEntityEnabled = false;

    @Column(name = "is_automatic_transfer_enabled")
    private Boolean isAutomaticTransferEnabled = false;

    @Column(name = "events_revamp_enabled")
    private Boolean eventsRevampEnabled;

    @Column(name = "is_always_utilization")
    private Boolean isAlwaysUtilization;

    @Column(name = "is_utilization_for_container_quoted")
    private Boolean isUtilizationForContainerQuoted;

    @Column(name = "has_no_utilization")
    private Boolean hasNoUtilization;

    @Column(name = "is_awb_revamp_enabled")
    private Boolean isAwbRevampEnabled;

    @Column(name = "fcr_document")
    private String fcrDocument;

    @Column(name = "is_runner_v3_enabled")
    private Boolean isRunnerV3Enabled;

}
