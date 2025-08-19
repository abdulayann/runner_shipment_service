package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.entity.enums.AirAuthorisingEntity;
import com.dpw.runner.shipment.services.entity.enums.AndesStatus;
import com.dpw.runner.shipment.services.entity.enums.LGDStatus;
import com.dpw.runner.shipment.services.entity.enums.Ownership;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Generated
public class AdditionalDetailExternalResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime expiryDate;
    private String inspection;
    private String airwayBillDims;
    private String shipperCODPM;
    private BigDecimal spotRate;
    private String efreightStatus;
    private String sci;
    private Boolean importExportShipmentLock;
    private AirAuthorisingEntity securityStatusReceivedFrom;
    private String additionalSecurityInformation;
    private String customLocation;
    private String customCity;
    private Boolean isImportClearance;
    private Boolean isExportClearance;
    private String IECode;
    private String branchSINumber;
    private AndesStatus andesStatus;
    private String andesStatusResponseText;
    private String tipoDocumentConsignor;
    private String tipoDocumentNotifyParty;
    private String tipoDocumentConsignee;
    private String andesTicket;
    private Long hsnNumber;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime inwardDateAndTime;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime IGMInwardDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime IGMFileDate;
    private Long lineNumber;
    private Long subLineNumber;
    private Long localLineNumber;
    @JsonProperty("SMTPIGMNumber")
    private String SMTPIGMNumber;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @JsonProperty("SMTPIGMDate")
    private LocalDateTime SMTPIGMDate;
    private PartiesExternalResponse ownershipOrg;
    private Ownership ownership;
    private String ownershipName;
    private Ownership passedBy;
    private String passedByPerson;
    private Boolean isCmsHBLSent;
    private Boolean isCreditOverrideApproved;
    private BigDecimal freeDays;
    private String customHouse;
    private String supplierInvoiceNumber;
    private BigDecimal invoiceValue;
    private BigDecimal assessValue;
    private BigDecimal CIFValue;
    private BigDecimal totalDuty;
    private String externalNotes;
    private Long bondedWarehouseId;
    private String releaseType;
    private String houseBillType;
    private String onBoard;
    private String deliveryMode;
    private Integer original;
    private Integer copy;
    private String BLChargesDisplay;
    private String BLExporterShipment;
    private List<String> screeningStatus;
    private String paidPlace;
    private String placeOfIssue;
    private String placeOfSupply;
    private String goodsCO;
    private String BOENumber;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime BOEDate;
    private Boolean printedOriginal;
    private Boolean WBLPrinted;
    private Boolean draftPrinted;
    private Boolean surrenderPrinted;
    private String importBrokerCountry;
    private String exportBrokerCountry;

    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime customsNoIssueDate;
    private BigDecimal shipperCOD;
    private String phase;
    private String ADCode;
    private String BEType;
    private String spotRateType;
    private String CHAJobNumber;
    private String regulatedEntityCategory;
    private String IGMFileNo;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime andesResponseDate;
    private String peruEntryExitPoint;
    private Long warehouseId;
    private String activityType;
    private Boolean isInland;
    private LGDStatus lgdStatus;

    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime estimatedPickupDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime supplierInvoiceDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime onBoardDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime dateOfReceipt;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime dateOfIssue;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime shippedOnboard;

    private PartiesExternalResponse sendingForwarder;
    private PartiesExternalResponse receivingForwarder;
    private PartiesExternalResponse traderOrSupplier;
    private PartiesExternalResponse eTailor;
    private PartiesExternalResponse receivingAgent;
    private PartiesExternalResponse notifyParty;
    private PartiesExternalResponse importBroker;
    private PartiesExternalResponse exportBroker;
    private PartiesExternalResponse borrowedFrom;
    private PartiesExternalResponse sendingAgent;
    private Map<String, String> tenantIdsData;
    private Map<String, String> unlocationData;
    private Map<String, String> masterData;
    private String customDeclType;
    private String agentReference;
    private String cargoTerms;
    private String bLTermsandConditionsId;
    private String blComments;
    private String cargoTermsDescription;
    private String bLRemarksDescription;
    private String bLRemarks;
    private Boolean isSummaryUpdated;
    private String summary;
    private Map<String, String> textData;
    private String exemptionCodes;
    private String aomFreeText;
    private String emergencyContactNumber;
    private String emergencyContactNumberCode;
    private Boolean docTurnedOverToCustomer;
    private Boolean pickupByConsigneeCompleted;
    private Boolean emptyContainerReturned;
    private Boolean isExportCustomClearanceCompleted;
    private Integer fcrNumber;

    private LocalDateTime pickupDate;
    private LocalDateTime customReleaseDate;
    private LocalDateTime cargoDeliveredDate;
    private LocalDateTime warehouseCargoArrivalDate;
    private LocalDateTime cargoOutForDelivery;
    private LocalDateTime proofOfDeliveryDate;
    private LocalDateTime blInstructionReceived;

    public void addTextData(Map<String, String> dataMap) {
        if(textData == null) {
            textData = new HashMap<>();
        }
        textData.putAll(dataMap);
    }
}

