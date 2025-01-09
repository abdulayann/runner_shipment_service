package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.entity.enums.AirAuthorisingEntity;
import com.dpw.runner.shipment.services.entity.enums.AndesStatus;
import com.dpw.runner.shipment.services.entity.enums.LGDStatus;
import com.dpw.runner.shipment.services.entity.enums.Ownership;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.*;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.BatchSize;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import javax.validation.constraints.Size;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Table(name = "shipment_additional_details")
@Getter
@Setter
@Data
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
@SQLDelete(sql = "UPDATE shipment_additional_details SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class AdditionalDetails extends MultiTenancy {

    @Column(name = "customs_no_issue_date")
    private LocalDateTime customsNoIssueDate;

    @Column(name = "expiry_date")
    private LocalDateTime expiryDate;

    @Size(max=3, message = "max size is 3 for inspection")
    @Column(name = "inspection")
    private String inspection;

    @Size(max=3, message = "max size is 3 for airway_bill_dims")
    @Column(name = "airway_bill_dims")
    @MasterData(type = MasterDataType.AIRWAY_BILL_DIMS)
    private String airwayBillDims;

    @Column(name = "shipper_cod")
    private BigDecimal shipperCOD;

    @Column(name = "shipper_cod_pm")
    @Size(max = 3, message = "max size is 3 for shipper_cod_pm")
    @MasterData(type = MasterDataType.SHIPPER_COD_TYPE)
    private String shipperCODPM;

    @Size(max=3, message = "max size is 3 for phase")
    @Column(name = "phase")
    @MasterData(type = MasterDataType.PHASE)
    private String phase;

    @Column(name = "spot_rate")
    private BigDecimal spotRate;

    @Size(max=3, message = "max size is 3 for spot_rate_type")
    @Column(name = "spot_rate_type")
    @MasterData(type = MasterDataType.SPOT_RATE_TYPE)
    private String spotRateType;

    @Size(max=3, message = "max size is 3 for efreight_status")
    @Column(name = "efreight_status")
    @MasterData(type = MasterDataType.EFREIGHT_STATUS)
    private String efreightStatus;

    @Column(name = "sci")
    @MasterData(type = MasterDataType.SCI)
    private String sci;

    @Column(name = "import_export_shipment_lock")
    private Boolean importExportShipmentLock;

    @Size(max=20, message = "max size is 20 for cha_job_number")
    @Column(name = "cha_job_number")
    private String CHAJobNumber;

    @Size(max=10, message = "max size is 10 for ad_code")
    @Column(name = "ad_code")
    private String ADCode;

    @Column(name = "be_type")
    private String BEType;

    @Column(name = "security_status_received_from")
    @Enumerated(EnumType.STRING)
    private AirAuthorisingEntity securityStatusReceivedFrom;

    @Column(name = "additional_security_information")
    private String additionalSecurityInformation;

    @Column(name = "regulated_entity_category")
    private String regulatedEntityCategory;

    @Column(name = "custom_location")
    @MasterData(type = MasterDataType.CUSTOM_LOCATION, cascade = Constants.CUSTOM_CITY)
    private String customLocation;

    @Column(name = "custom_city")
    @MasterData(type = MasterDataType.CUSTOM_CITY)
    private String customCity;

    @Column(name = "is_import_clearance")
    private Boolean isImportClearance;

    @Column(name = "is_export_clearance")
    private Boolean isExportClearance;

    @Column(name = "igm_file_no")
    private String IGMFileNo;

    @Column(name = "ie_code")
    private String IECode;

    @Column(name = "branch_si_number")
    private String branchSINumber;

    @Enumerated(EnumType.STRING)
    @Column(name = "andes_status")
    private AndesStatus andesStatus;

    @Column(name = "andes_response_date")
    private LocalDateTime andesResponseDate;

    @Column(name = "andes_status_response_text")
    private String andesStatusResponseText;

    @Column(name = "peru_entry_exit_point")
    private String peruEntryExitPoint;

    @Column(name = "tipo_document_notify_party")
    private String tipoDocumentNotifyParty;

    @Column(name = "tipo_document_consignee")
    private String tipoDocumentConsignee;

    @Column(name = "tipo_document_consignor")
    private String tipoDocumentConsignor;

    @Column(name = "andes_ticket")
    private String andesTicket;

    @Column(name = "warehouse_id")
    @DedicatedMasterData(type = Constants.WARE_HOUSE_DATA)
    private Long warehouseId;

    @Column(name = "activity_type")
    @DedicatedMasterData(type = Constants.ACTIVITY_TYPE)
    private String activityType;

    @Column(name = "hsn_number")
    private Long hsnNumber;

    @Column(name = "igm_file_date")
    private LocalDateTime IGMFileDate;

    @Column(name = "igm_inward_date")
    private LocalDateTime IGMInwardDate;

    @Column(name = "inward_date_and_time")
    private LocalDateTime inwardDateAndTime;

    @Column(name = "line_number")
    private Long lineNumber;

    @Column(name = "sub_line_number")
    private Long subLineNumber;

    @Column(name = "local_line_number")
    private Long localLineNumber;

    @Size(max=10, message = "max size is 10 for smtp_igm_number")
    @Column(name = "smtp_igm_number")
    @JsonProperty("SMTPIGMNumber")
    private String SMTPIGMNumber;

    @Column(name = "smtp_igm_date")
    @JsonProperty("SMTPIGMDate")
    private LocalDateTime SMTPIGMDate;

    @Column(name = "is_inland")
    private Boolean isInland;

    @Enumerated(EnumType.STRING)
    @Column(name = "ownership")
    private Ownership ownership;

    @Column(name = "ownership_name")
    @TenantIdData
    private String ownershipName;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "ownership_org", referencedColumnName = "id")
    @OrganizationData
    private Parties ownershipOrg;

    @Enumerated(EnumType.STRING)
    @Column(name = "passed_by")
    private Ownership passedBy;

    @Column(name = "passed_by_person")
    private String passedByPerson;

    @Enumerated(EnumType.STRING)
    @Column(name = "lgd_status")
    private LGDStatus lgdStatus;

    @Column(name = "is_cms_hbl_sent")
    private Boolean isCmsHBLSent;

    @Column(name = "is_credit_override_approved")
    private Boolean isCreditOverrideApproved;

    @Column(name = "free_days")
    private BigDecimal freeDays;

    @Column(name = "custom_house")
    private String customHouse;

    @Column(name = "supplier_invoice_number")
    private String supplierInvoiceNumber;

    @Column(name = "supplier_invoice_date")
    private LocalDateTime supplierInvoiceDate;

    @Column(name = "invoice_value")
    private BigDecimal invoiceValue;

    @Column(name = "assess_value")
    private BigDecimal assessValue;

    @Column(name = "cif_value")
    private BigDecimal CIFValue;

    @Column(name = "total_duty")
    private BigDecimal totalDuty;

    @Size(max=256, message = "max size is 256 for external_notes")
    @Column(name = "external_notes")
    private String externalNotes;

    @Column(name = "bonded_warehouse_id")
    @DedicatedMasterData(type = Constants.WARE_HOUSE_DATA)
    private Long bondedWarehouseId;

    @Size(max=3, message = "max size is 3 for release_type")
    @Column(name = "release_type")
    @MasterData(type = MasterDataType.RELEASE_TYPE)
    private String releaseType;

    @Size(max=3, message = "max size is 3 for house_bill_type")
    @Column(name = "house_bill_type")
    @MasterData(type = MasterDataType.HOUSE_BILL_TYPE)
    private String houseBillType;

    @Column(name = "on_board")
    @Size(max=3, message = "max size is 3 for on_board")
    @MasterData(type = MasterDataType.ON_BOARD)
    private String onBoard;

    @Column(name = "on_board_date")
    private LocalDateTime onBoardDate;

    @Column(name = "delivery_mode_id")
    @MasterData(type = MasterDataType.HBL_DELIVERY_MODE)
    private String deliveryMode;

    @Column(name = "original")
    private Integer original;

    @Column(name = "copy")
    private Integer copy;

    @Column(name = "bl_charges_display")
    @MasterData(type = MasterDataType.CHARGES_APPLY)
    private String BLChargesDisplay;

    @Column(name = "bl_exporter_shipment")
    @MasterData(type = MasterDataType.EXPORTER_STMT)
    private String BLExporterShipment;

    @Column(name = "screening_status")
    @Size(max=50, message = "max size is 50 for screening_status")
    //@MasterData(type = MasterDataType.SCREENING_STATUS)
    @ElementCollection(targetClass = String.class, fetch = FetchType.EAGER)
    @CollectionTable(name = "screening_status", joinColumns = @JoinColumn(name = "shipment_additional_details_id"))
    @BatchSize(size = 50)
    private List<String> screeningStatus;

    @Column(name = "paid_place")
    @UnlocationData
    private String paidPlace;

    @Column(name = "place_of_issue")
    @UnlocationData
    private String placeOfIssue;

    @Column(name = "place_of_supply")
    @UnlocationData
    private String placeOfSupply;

    @Column(name = "date_of_issue")
    private LocalDateTime dateOfIssue;

    @Column(name = "date_of_receipt")
    private LocalDateTime dateOfReceipt;

    @Column(name = "goods_co_id")
    @MasterData(type = MasterDataType.COUNTRIES)
    private String goodsCO;

    @Column(name = "boe_number")
    private String BOENumber;

    @Column(name = "boe_date")
    private LocalDateTime BOEDate;

    @Column(name = "printed_original")
    private Boolean printedOriginal;

    @Column(name = "wbl_printed")
    private Boolean WBLPrinted;

    @Column(name = "draft_printed")
    private Boolean draftPrinted;

    @Column(name = "surrender_printed")
    private Boolean surrenderPrinted;

    @MasterData(type = MasterDataType.COUNTRIES)
    @Column(name = "import_broker_country")
    private String importBrokerCountry;

    @MasterData(type = MasterDataType.COUNTRIES)
    @Column(name = "export_broker_country")
    private String exportBrokerCountry;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "notify_party_id", referencedColumnName = "id")
    @OrganizationData
    private Parties notifyParty;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "import_broker_id", referencedColumnName = "id")
    @OrganizationData
    private Parties importBroker;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "export_broker_id", referencedColumnName = "id")
    @OrganizationData
    private Parties exportBroker;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "sending_forwarder_id", referencedColumnName = "id")
    @OrganizationData
    private Parties sendingForwarder;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "receiving_forwarder_id", referencedColumnName = "id")
    @OrganizationData
    private Parties receivingForwarder;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "trader_or_supplier_id", referencedColumnName = "id")
    @OrganizationData
    private Parties traderOrSupplier;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "etailor_id", referencedColumnName = "id")
    @OrganizationData
    private Parties eTailor;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "borrowed_from_id", referencedColumnName = "id")
    @OrganizationData
    private Parties borrowedFrom;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "sending_agent_id", referencedColumnName = "id")
    @OrganizationData
    private Parties sendingAgent;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "receiving_agent_id", referencedColumnName = "id")
    @OrganizationData
    private Parties receivingAgent;

    @Column(name = "shipment_id")
    private Long shipmentId;

    @Column(name = "custom_decl_type")
    @MasterData(type = MasterDataType.CUSTOM_DECL_TYPE)
    private String customDeclType;

    @Column(name = "agent_reference")
    private String agentReference;

    @Column(name = "bl_terms_and_conditions_id")
    @MasterData(type = MasterDataType.BL_TERMS_AND_CONDITIONS)
    @Size(max=16, message = "max size is 16 for bl_terms_and_conditions_id")
    private String bLTermsandConditionsId;

    @Column(name = "bl_comments")
    @Size(max=2500, message = "max size is 2500 for bl_comments")
    private String blComments;

    @Column(name = "cargo_terms")
    @MasterData(type = MasterDataType.BL_CARGO_TERMS)
    @Size(max=16, message = "max size is 16 for cargo_terms")
    private String cargoTerms;

    @Column(name = "cargo_terms_description")
    @Size(max=2500, message = "max size is 2500 for cargo_terms_description")
    private String cargoTermsDescription;

    @Column(name = "bl_remarks")
    @MasterData(type = MasterDataType.BL_REMARKS)
    @Size(max=16, message = "max size is 16 for bl_remarks")
    private String bLRemarks;

    @Column(name = "bl_remarks_description")
    @Size(max=2500, message = "max size is 2500 for bl_remarks_description")
    private String bLRemarksDescription;

    @Column(name = "summary")
    @Size(max = 2048, message = "max size is 2048 for summary")
    private String summary;

    @Column(name = "is_summary_updated")
    private Boolean isSummaryUpdated;

    @Column(name = "exemption_codes")
    @MasterData(type = MasterDataType.EXEMPTION_CODES)
    private String exemptionCodes;

    @Column(name = "aom_free_text")
    private String aomFreeText;

    @Column(name = "emergency_contact_number")
    @Size(max=31, message = "max size is 31 for emergency_contact_number")
    private String emergencyContactNumber;

    @Column(name = "emergency_contact_number_code")
    @Size(max=31, message = "max size is 31 for emergency_contact_number_code")
    private String emergencyContactNumberCode;

    @Column(name = "pickup_date")
    private LocalDateTime pickupDate;

    @Column(name = "cargo_delivered_date")
    private LocalDateTime cargoDeliveredDate;

    @Column(name = "custom_release_date")
    private LocalDateTime customReleaseDate;

    @Column(name = "doc_turned_over_to_customer")
    private Boolean docTurnedOverToCustomer;

    @Column(name = "proof_of_delivery_date")
    private LocalDateTime proofOfDeliveryDate;

    @Column(name = "warehouse_cargo_arrival_date")
    private LocalDateTime warehouseCargoArrivalDate;

    @Column(name = "pickup_by_consignee_completed")
    private Boolean pickupByConsigneeCompleted;

    @Column(name = "empty_container_returned")
    private Boolean emptyContainerReturned;

    @Column(name = "is_export_custom_clearance_completed")
    private Boolean isExportCustomClearanceCompleted;

    @Column(name = "bl_instruction_received")
    private LocalDateTime blInstructionReceived;

    @Column(name = "cargo_out_for_delivery")
    private LocalDateTime cargoOutForDelivery;

//    @Column(name = "fcr_number")
//    private Integer fcrNumber = 0;
}
