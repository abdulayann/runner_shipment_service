package com.dpw.runner.shipment.services.dto.v3.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.entity.enums.AirAuthorisingEntity;
import com.dpw.runner.shipment.services.entity.enums.AndesStatus;
import com.dpw.runner.shipment.services.entity.enums.LGDStatus;
import com.dpw.runner.shipment.services.entity.enums.Ownership;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@Data
@ApiModel("Shipment Additional Details Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class AdditionalDetailV3Request extends CommonRequest implements IRunnerRequest {
    private Long id;
    private LocalDateTime customsNoIssueDate;
    private LocalDateTime expiryDate;
    private String inspection;
    private String airwayBillDims;
    private BigDecimal shipperCOD;
    private String shipperCODPM;
    private String phase;
    private BigDecimal spotRate;
    private String spotRateType;
    private String efreightStatus;
    private String sci;
    private Boolean importExportShipmentLock;
    private String CHAJobNumber;
    private String ADCode;
    private String BEType;
    private AirAuthorisingEntity securityStatusReceivedFrom;
    private String additionalSecurityInformation;
    private String regulatedEntityCategory;
    private String customLocation;
    private String customCity;
    private Boolean isImportClearance;
    private Boolean isExportClearance;
    private String IGMFileNo;
    private String IECode;
    private String branchSINumber;
    private AndesStatus andesStatus;
    private LocalDateTime andesResponseDate;
    private String andesStatusResponseText;
    private String peruEntryExitPoint;
    private String tipoDocumentNotifyParty;
    private String tipoDocumentConsignee;
    private String tipoDocumentConsignor;
    private String andesTicket;
    private Long warehouseId;
    private String activityType;
    private Long hsnNumber;
    private LocalDateTime IGMFileDate;
    private LocalDateTime IGMInwardDate;
    private LocalDateTime inwardDateAndTime;
    private Long lineNumber;
    private Long subLineNumber;
    private Long localLineNumber;
    @JsonProperty("SMTPIGMNumber")
    private String SMTPIGMNumber;
    @JsonProperty("SMTPIGMDate")
    private LocalDateTime SMTPIGMDate;
    private Boolean isInland;
    private Ownership ownership;
    private String ownershipName;
    private PartiesRequest ownershipOrg;
    private Ownership passedBy;
    private String passedByPerson;
    private LGDStatus lgdStatus;
    private Boolean isCmsHBLSent;
    private Boolean isCreditOverrideApproved;
    private BigDecimal freeDays;
    private String customHouse;
    private String supplierInvoiceNumber;
    private LocalDateTime supplierInvoiceDate;
    private BigDecimal invoiceValue;
    private BigDecimal assessValue;
    private BigDecimal CIFValue;
    private BigDecimal totalDuty;
    private String externalNotes;
    private Long bondedWarehouseId;
    private String releaseType;
    private String houseBillType;
    private String onBoard;
    private LocalDateTime onBoardDate;
    private String deliveryMode;
    private Integer original;
    private Integer copy;
    private String BLChargesDisplay;
    private String BLExporterShipment;
    private List<String> screeningStatus;
    private String paidPlace;
    private String placeOfIssue;
    private String placeOfSupply;
    private LocalDateTime dateOfIssue;
    private LocalDateTime dateOfReceipt;
    private String goodsCO;
    private String BOENumber;
    private LocalDateTime BOEDate;
    private Boolean printedOriginal;
    private Boolean WBLPrinted;
    private Boolean draftPrinted;
    private Boolean surrenderPrinted;
    private String importBrokerCountry;
    private String exportBrokerCountry;
    private PartiesRequest notifyParty;
    private PartiesRequest importBroker;
    private PartiesRequest exportBroker;
    private PartiesRequest sendingForwarder;
    private PartiesRequest receivingForwarder;
    private PartiesRequest traderOrSupplier;
    private PartiesRequest eTailor;
    private PartiesRequest borrowedFrom;
    private PartiesRequest sendingAgent;
    private PartiesRequest receivingAgent;
    private String customDeclType;
    private String agentReference;
    private String bLTermsandConditionsId;
    private String blComments;
    private String cargoTerms;
    private String cargoTermsDescription;
    private String bLRemarks;
    private String bLRemarksDescription;
    private String summary;
    private Boolean isSummaryUpdated;
    private String exemptionCodes;
    private String aomFreeText;
    private String emergencyContactNumber;
    private String emergencyContactNumberCode;
    @ExcludeTimeZone
    private LocalDateTime pickupDate;
    @ExcludeTimeZone
    private LocalDateTime cargoDeliveredDate;
    @ExcludeTimeZone
    private LocalDateTime estimatedPickupDate;
    @ExcludeTimeZone
    private LocalDateTime estimatedCargoDeliveredDate;
    private LocalDateTime customReleaseDate;
    private Boolean docTurnedOverToCustomer;
    private LocalDateTime proofOfDeliveryDate;
    private LocalDateTime warehouseCargoArrivalDate;
    private Boolean pickupByConsigneeCompleted;
    private Boolean emptyContainerReturned;
    private Boolean isExportCustomClearanceCompleted;
    private LocalDateTime blInstructionReceived;
    private LocalDateTime cargoOutForDelivery;
    private Integer fcrNumber = 0;
}
