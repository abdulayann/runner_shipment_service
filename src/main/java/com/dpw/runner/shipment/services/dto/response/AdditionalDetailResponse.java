package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateSerializer;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.entity.enums.AirAuthorisingEntity;
import com.dpw.runner.shipment.services.entity.enums.AndesStatus;
import com.dpw.runner.shipment.services.entity.enums.LGDStatus;
import com.dpw.runner.shipment.services.entity.enums.Ownership;
import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDate;
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
public class AdditionalDetailResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime customsNoIssueDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
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
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
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
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime IGMFileDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime IGMInwardDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime inwardDateAndTime;
    private Long lineNumber;
    private Long subLineNumber;
    private Long localLineNumber;
    @JsonProperty("SMTPIGMNumber")
    private String SMTPIGMNumber;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @JsonProperty("SMTPIGMDate")
    private LocalDateTime SMTPIGMDate;
    private Boolean isInland;
    private Ownership ownership;
    private String ownershipName;
    private PartiesResponse ownershipOrg;
    private Ownership passedBy;
    private String passedByPerson;
    private LGDStatus lgdStatus;
    private Boolean isCmsHBLSent;
    private Boolean isCreditOverrideApproved;
    private BigDecimal freeDays;
    private String customHouse;
    private String supplierInvoiceNumber;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
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
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
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
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime dateOfIssue;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime dateOfReceipt;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime shippedOnboard;
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
    private PartiesResponse notifyParty;
    private PartiesResponse importBroker;
    private PartiesResponse exportBroker;
    private PartiesResponse sendingForwarder;
    private PartiesResponse receivingForwarder;
    private PartiesResponse traderOrSupplier;
    private PartiesResponse eTailor;
    private PartiesResponse borrowedFrom;
    private PartiesResponse sendingAgent;
    private PartiesResponse receivingAgent;
    private Map<String, String> masterData;
    private Map<String, String> unlocationData;
    private Map<String, String> tenantIdsData;
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
    private Map<String, String> textData;
    private String exemptionCodes;
    private String aomFreeText;
    private String emergencyContactNumber;
    private String emergencyContactNumberCode;
    private LocalDateTime pickupDate;
    private LocalDateTime cargoDeliveredDate;
    private LocalDateTime customReleaseDate;
    private Boolean docTurnedOverToCustomer;
    private LocalDateTime proofOfDeliveryDate;
    private LocalDateTime warehouseCargoArrivalDate;
    private Boolean pickupByConsigneeCompleted;
    private Boolean emptyContainerReturned;
    private Boolean isExportCustomClearanceCompleted;
    private LocalDateTime blInstructionReceived;
    private LocalDateTime cargoOutForDelivery;
    private Integer fcrNumber;

    public void addTextData(Map<String, String> dataMap) {
        if(textData == null) {
            textData = new HashMap<>();
        }
        textData.putAll(dataMap);
    }
}
