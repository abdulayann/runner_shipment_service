package com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel;

import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.config.LocalDateTimeWithTimeZoneSerializer;
import com.dpw.runner.shipment.services.entity.enums.AirAuthorisingEntity;
import com.dpw.runner.shipment.services.entity.enums.AndesStatus;
import com.dpw.runner.shipment.services.entity.enums.LGDStatus;
import com.dpw.runner.shipment.services.entity.enums.Ownership;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class AdditionalDetailModel implements IDocumentModel {
    @JsonProperty("Id")
    private Long id;
    @JsonProperty("CustomsNoIssueDate")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime customsNoIssueDate;
    @JsonProperty("ExpiryDate")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime expiryDate;
    @JsonProperty("Inspection")
    private String inspection;
    @JsonProperty("AirwayBillDims")
    private String airwayBillDims;
    @JsonProperty("ShipperCOD")
    private BigDecimal shipperCOD;
    @JsonProperty("ShipperCODPM")
    private String shipperCODPM;
    @JsonProperty("Phase")
    private String phase;
    @JsonProperty("SpotRate")
    private BigDecimal spotRate;
    @JsonProperty("SpotRateType")
    private String spotRateType;
    @JsonProperty("EfreightStatus")
    private String efreightStatus;
    @JsonProperty("ImportExportShipmentLock")
    private Boolean importExportShipmentLock;
    @JsonProperty("CHAJobNumber")
    private String CHAJobNumber;
    @JsonProperty("ADCode")
    private String ADCode;
    @JsonProperty("BEType")
    private String BEType;
    @JsonProperty("CustomLocation")
    private String customLocation;
    @JsonProperty("CustomCity")
    private String customCity;
    @JsonProperty("IsImportClearance")
    private Boolean isImportClearance;
    @JsonProperty("IsExportClearance")
    private Boolean isExportClearance;
    @JsonProperty("IGMFileNo")
    private String IGMFileNo;
    @JsonProperty("IECode")
    private String IECode;
    @JsonProperty("BranchSINumber")
    private String branchSINumber;
    @JsonProperty("AndesStatus")
    private AndesStatus andesStatus;
    @JsonProperty("AndesResponseDate")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime andesResponseDate;
    @JsonProperty("AndesStatusResponseText")
    private String andesStatusResponseText;
    @JsonProperty("PeruEntryExitPoint")
    private String peruEntryExitPoint;
    @JsonProperty("TipoDocumentNotifyParty")
    private String tipoDocumentNotifyParty;
    @JsonProperty("TipoDocumentConsignee")
    private String tipoDocumentConsignee;
    @JsonProperty("TipoDocumentConsignor")
    private String tipoDocumentConsignor;
    @JsonProperty("AndesTicket")
    private String andesTicket;
    @JsonProperty("WarehouseId")
    private Long warehouseId;
    @JsonProperty("ActivityType")
    private String activityType;
    @JsonProperty("HsnNumber")
    private Long hsnNumber;
    @JsonProperty("IGMFileDate")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime IGMFileDate;
    @JsonProperty("IGMInwardDate")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime IGMInwardDate;
    @JsonProperty("InwardDateAndTime")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime inwardDateAndTime;
    @JsonProperty("LineNumber")
    private Long lineNumber;
    @JsonProperty("SubLineNumber")
    private Long subLineNumber;
    @JsonProperty("LocalLineNumber")
    private Long localLineNumber;
    @JsonProperty("SMTPIGMNumber")
    private String SMTPIGMNumber;
    @JsonProperty("SMTPIGMDate")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime SMTPIGMDate;
    @JsonProperty("IsInland")
    private Boolean isInland;
    @JsonProperty("Ownership")
    private Ownership ownership;
    @JsonProperty("OwnershipName")
    private String ownershipName;
    @JsonProperty("OwnershipOrg")
    private PartiesModel ownershipOrg;
    @JsonProperty("PassedBy")
    private Ownership passedBy;
    @JsonProperty("PassedByPerson")
    private String passedByPerson;
    @JsonProperty("LGDStatus")
    private LGDStatus lgdStatus;
    @JsonProperty("IsCmsHBLSent")
    private Boolean isCmsHBLSent;
    @JsonProperty("IsCreditOverrideApproved")
    private Boolean isCreditOverrideApproved;
    @JsonProperty("FreeDays")
    private BigDecimal freeDays;
    @JsonProperty("CustomHouse")
    private String customHouse;
    @JsonProperty("SupplierInvoiceNumber")
    private String supplierInvoiceNumber;
    @JsonProperty("SupplierInvoiceDate")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime supplierInvoiceDate;
    @JsonProperty("InvoiceValue")
    private BigDecimal invoiceValue;
    @JsonProperty("AssessValue")
    private BigDecimal assessValue;
    @JsonProperty("CIFValue")
    private BigDecimal CIFValue;
    @JsonProperty("TotalDuty")
    private BigDecimal totalDuty;
    @JsonProperty("ExternalNotes")
    private String externalNotes;
    @JsonProperty("BondedWarehouseId")
    private Long bondedWarehouseId;
    @JsonProperty("ReleaseType")
    private String releaseType;
    @JsonProperty("HouseBillType")
    private String houseBillType;
    @JsonProperty("OnBoard")
    private String onBoard;
    @JsonProperty("OnBoardDate")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime onBoardDate;
    @JsonProperty("DeliveryMode")
    private String deliveryMode;
    @JsonProperty("Original")
    private Integer original;
    @JsonProperty("Copy")
    private Integer copy;
    @JsonProperty("BLChargesDisplay")
    private String BLChargesDisplay;
    @JsonProperty("BLExporterShipment")
    private String BLExporterShipment;
    @JsonProperty("ScreeningStatus")
    private List<String> screeningStatus;
    @JsonProperty("PaidPlace")
    private String paidPlace;
    @JsonProperty("PlaceOfIssue")
    private String placeOfIssue;
    @JsonProperty("PlaceOfSupply")
    private String placeOfSupply;
    @JsonProperty("DateOfIssue")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime dateOfIssue;
    @JsonProperty("DateOfReceipt")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime dateOfReceipt;
    @JsonProperty("GoodsCO")
    private String goodsCO;
    @JsonProperty("BOENumber")
    private String BOENumber;
    @JsonProperty("BOEDate")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime BOEDate;
    @JsonProperty("PrintedOriginal")
    private Boolean printedOriginal;
    @JsonProperty("WBLPrinted")
    private Boolean WBLPrinted;
    @JsonProperty("DraftPrinted")
    private Boolean draftPrinted;
    @JsonProperty("SurrenderPrinted")
    private Boolean surrenderPrinted;
    @JsonProperty("NotifyParty")
    private PartiesModel notifyParty;
    @JsonProperty("ImportBroker")
    private PartiesModel importBroker;
    @JsonProperty("ExportBroker")
    private PartiesModel exportBroker;
    @JsonProperty("SendingForwarder")
    private PartiesModel sendingForwarder;
    @JsonProperty("ReceivingForwarder")
    private PartiesModel receivingForwarder;
    @JsonProperty("TraderOrSupplier")
    private PartiesModel traderOrSupplier;
    @JsonProperty("ETailor")
    private PartiesModel eTailor;
    @JsonProperty("BorrowedFrom")
    private PartiesModel borrowedFrom;
    @JsonProperty("SendingAgent")
    private PartiesModel sendingAgent;
    @JsonProperty("ReceivingAgent")
    private PartiesModel receivingAgent;
    private String agentReference;
    private String cargoTermsDescription;
    private String bLRemarks;
    private String bLRemarksDescription;
    private String exemptionCodes;
    private String aomFreeText;
    @JsonProperty("EmergencyContactNumber")
    private String emergencyContactNumber;
    @JsonProperty("EmergencyContactNumberCode")
    private String emergencyContactNumberCode;
    @JsonProperty("Sci")
    private String sci;

    // new CSD fields
    @JsonProperty("SecurityStatusReceivedFrom")
    private AirAuthorisingEntity securityStatusReceivedFrom;
    @JsonProperty("AdditionalSecurityInformation")
    private String additionalSecurityInformation;
    @JsonProperty("RegulatedEntityCategory")
    private String regulatedEntityCategory;
    @JsonProperty("FcrNumber")
    private Integer fcrNumber;
}
