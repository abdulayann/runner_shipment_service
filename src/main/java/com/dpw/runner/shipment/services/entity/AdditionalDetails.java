package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.enums.AndesStatus;
import com.dpw.runner.shipment.services.entity.enums.LGDStatus;
import com.dpw.runner.shipment.services.entity.enums.Ownership;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.*;
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

    private String inspection;

    @Column(name = "airway_bill_dims")
    private String airwayBillDims;

    @Column(name = "shipper_cod")
    private BigDecimal shipperCOD;

    @Column(name = "shipper_cod_pm")
    private String shipperCODPM;

    @Column(name = "phase")
    private String phase;

    @Column(name = "spot_rate")
    private BigDecimal spotRate;

    @Column(name = "spot_rate_type")
    private String spotRateType;

    @Column(name = "efreight_status")
    private String efreightStatus;

    @Column(name = "import_export_shipment_lock")
    private Boolean importExportShipmentLock;

    @Column(name = "cha_job_number")
    private String CHAJobNumber;

    @Column(name = "ad_code")
    private String ADCode;

    @Column(name = "be_type")
    private String BEType;

    @Column(name = "custom_location")
    private String customLocation;

    @Column(name = "custom_city")
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

    @Column(name = "external_notes")
    private String externalNotes;

    @Column(name = "bonded_warehouse_id")
    private Long bondedWarehouseId;

    @Column(name = "release_type")
    private String releaseType;

    @Column(name = "house_bill_type")
    private String houseBillType;

    @Column(name = "on_board")
    private String onBoard;

    @Column(name = "on_board_date")
    private LocalDateTime onBoardDate;

    @Column(name = "delivery_mode_id")
    private String deliveryMode;

    @Column(name = "original")
    private Integer original;

    @Column(name = "copy")
    private Integer copy;

    @Column(name = "bl_charges_display")
    private String BLChargesDisplay;

    @Column(name = "bl_exporter_shipment")
    private String BLExporterShipment;

    @Column(name = "screening_status")
    private String screeningStatus;

    @Column(name = "paid_place")
    private String paidPlace;

    @Column(name = "place_of_issue")
    private String placeOfIssue;

    @Column(name = "place_of_supply")
    private String placeOfSupply;

    @Column(name = "date_of_issue")
    private LocalDateTime dateOfIssue;

    @Column(name = "date_of_receipt")
    private LocalDateTime dateOfReceipt;

    @Column(name = "goods_co_id")
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

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "notify_party_id", referencedColumnName = "id")
    private Parties notifyParty;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "import_broker_id", referencedColumnName = "id")
    private Parties importBroker;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "export_broker_id", referencedColumnName = "id")
    private Parties exportBroker;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "sending_forwarder_id", referencedColumnName = "id")
    private Parties sendingForwarder;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "receiving_forwarder_id", referencedColumnName = "id")
    private Parties receivingForwarder;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "trader_or_supplier_id", referencedColumnName = "id")
    private Parties traderOrSupplier;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "etailor_id", referencedColumnName = "id")
    private Parties eTailor;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "borrowed_from_id", referencedColumnName = "id")
    private Parties borrowedFrom;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "sending_agent_id", referencedColumnName = "id")
    private Parties sendingAgent;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "receiving_agent_id", referencedColumnName = "id")
    private Parties receivingAgent;

    @Column(name = "shipment_id")
    private Long shipmentId;
}
