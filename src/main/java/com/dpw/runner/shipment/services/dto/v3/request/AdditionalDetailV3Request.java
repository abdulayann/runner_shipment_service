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

import javax.validation.constraints.Size;
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
    @Size(max=3, message = "max size is 3 for inspection")
    private String inspection;
    @Size(max=3, message = "max size is 3 for airway bill dims")
    private String airwayBillDims;
    private BigDecimal shipperCOD;
    @Size(max = 3, message = "max size is 3 for shipper cod pm")
    private String shipperCODPM;
    @Size(max=3, message = "max size is 3 for phase")
    private String phase;
    private BigDecimal spotRate;
    @Size(max=3, message = "max size is 3 for spot rate type")
    private String spotRateType;
    @Size(max=3, message = "max size is 3 for efreight status")
    private String efreightStatus;
    private String sci;
    private Boolean importExportShipmentLock;
    @Size(max=20, message = "max size is 20 for cha job number")
    private String CHAJobNumber;
    @Size(max=10, message = "max size is 10 for ad code")
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
    @Size(max=10, message = "max size is 10 for smtp igm number")
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
    @Size(max=256, message = "max size is 256 for external notes")
    private String externalNotes;
    private Long bondedWarehouseId;
    @Size(max=3, message = "max size is 3 for release type")
    private String releaseType;
    @Size(max=3, message = "max size is 3 for house bill type")
    private String houseBillType;
    @Size(max=3, message = "max size is 3 for on board")
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
    @Size(max=16, message = "max size is 16 for bl terms and conditions id")
    private String bLTermsandConditionsId;
    @Size(max=2500, message = "max size is 2500 for bl comments")
    private String blComments;
    @Size(max=16, message = "max size is 16 for cargo terms")
    private String cargoTerms;
    @Size(max=2500, message = "max size is 2500 for cargo terms description")
    private String cargoTermsDescription;
    @Size(max=16, message = "max size is 16 for bl remarks")
    private String bLRemarks;
    @Size(max=2500, message = "max size is 2500 for bl remarks description")
    private String bLRemarksDescription;
    @Size(max = 2048, message = "max size is 2048 for summary")
    private String summary;
    private Boolean isSummaryUpdated;
    private String exemptionCodes;
    private String aomFreeText;
    @Size(max=31, message = "max size is 31 for emergency contact number")
    private String emergencyContactNumber;
    @Size(max=31, message = "max size is 31 for emergency contact number code")
    private String emergencyContactNumberCode;
    @ExcludeTimeZone
    private LocalDateTime pickupDate;
    @ExcludeTimeZone
    private LocalDateTime cargoDeliveredDate;
    @ExcludeTimeZone
    private LocalDateTime estimatedPickupDate;
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
