package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.entity.enums.AndesStatus;
import com.dpw.runner.shipment.services.entity.enums.LGDStatus;
import com.dpw.runner.shipment.services.entity.enums.Ownership;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.MasterData;
import com.dpw.runner.shipment.services.utils.OrganizationData;
import com.dpw.runner.shipment.services.utils.UnlocationData;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import javax.validation.constraints.Size;
import java.math.BigDecimal;
import java.time.LocalDateTime;

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
    private String inspection;

    @Size(max=3, message = "max size is 3 for airway_bill_dims")
    @Column(name = "airway_bill_dims")
    private String airwayBillDims;

    @Column(name = "shipper_cod")
    private BigDecimal shipperCOD;

    @Column(name = "shipper_cod_pm")
    @Size(max=3, message = "max size is 3 for shipper_cod_pm")
    private String shipperCODPM;

    @Size(max=3, message = "max size is 3 for phase")
    @Column(name = "phase")
    private String phase;

    @Size(max=3, message = "max size is 3 for spot_rate")
    @Column(name = "spot_rate")
    private BigDecimal spotRate;

    @Column(name = "spot_rate_type")
    private String spotRateType;

    @Size(max=3, message = "max size is 3 for efreight_status")
    @Column(name = "efreight_status")
    private String efreightStatus;

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
    private Long warehouseId;

    @Column(name = "activity_type")
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
    private String SMTPIGMNumber;

    @Column(name = "smtp_igm_date")
    private LocalDateTime SMTPIGMDate;

    @Column(name = "is_inland")
    private Boolean isInland;

    @Enumerated(EnumType.STRING)
    @Column(name = "ownership")
    private Ownership ownership;

    @Column(name = "ownership_name")
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
    @Size(max=3, message = "max size is 3 for screening_status")
    @MasterData(type = MasterDataType.SCREENING_STATUS)
    private String screeningStatus;

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
}
