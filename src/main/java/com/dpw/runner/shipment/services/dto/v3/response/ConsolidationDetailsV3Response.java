package com.dpw.runner.shipment.services.dto.v3.response;

import com.dpw.runner.shipment.services.commons.enums.TransportInfoStatus;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerSummaryResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.dto.response.AchievedQuantitiesResponse;
import com.dpw.runner.shipment.services.dto.response.AllocationsResponse;
import com.dpw.runner.shipment.services.dto.response.ArrivalDepartureDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.CarrierDetailResponse;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.response.ReferenceNumbersResponse;
import com.dpw.runner.shipment.services.dto.response.TriangulationPartnerResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentWtVolResponse;
import com.dpw.runner.shipment.services.entity.enums.AwbStatus;
import com.dpw.runner.shipment.services.entity.enums.MigrationStatus;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@NoArgsConstructor
@AllArgsConstructor
@Data
public class ConsolidationDetailsV3Response implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Integer tenantId;
    private String consolidationNumber;
    private String consolidationType;
    private String transportMode;
    private String containerCategory;
    private Boolean isDomestic;
    private String assignedTo;
    private String mawb;
    private String serviceLevel;
    private String payment;
    private String declarationType;
    private String deliveryMode;
    private Boolean isLinked;
    private Boolean isCharter;
    private String referenceNumber;
    private String packageType;
    private String agentReference;
    private String coLoadMBL;
    private String coLoadBookingReference;
    private String manifestPrint;
    private String printOtherDocs;
    private String awbDims;
    private String releaseType;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime masterBillIssueDate;
    private String dgClass;
    private String dgSubstance;
    private Boolean override;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime estimatedTerminalCutoff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime terminalCutoff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime verifiedGrossMassCutoff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime reeferCutoff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime bookingCutoff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime shipInstructionCutoff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime hazardousBookingCutoff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime latestFullEquDeliveredToCarrier;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime earliestDropOffFullEquToCarrier;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime earliestEmptyEquPickUp;
    private String volumeUtilization;
    private String weightUtilization;
    private String shipmentType;
    private String bol;
    private Boolean isCargoOnly;
    private Boolean isLocked;
    private String lockedBy;
    private String specialInstructions;
    private String description;
    private String marksnNums;
    private String additionalTerms;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime docsClosingTime;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime cargoClosingTime;
    private String mrnNumber;
    private String msnNumber;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime igmFileDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime igmInwardDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime inwardDateAndTime;
    private String igmFileNo;
    private String smtpigmNumber;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime smtpigmDate;
    private Boolean isInland;
    private Integer original;
    private Integer copy;
    private String doPlaceOfIssue;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime doIssueDate;
    private Long bondedWarehouseId;
    private Long warehouseId;
    private Long sourceTenantId;
    private Long parentTenantId;
    private String ediTransactionId;
    private List<TriangulationPartnerResponse> triangulationPartnerList;
    private Long triangulationPartner;
    private Long receivingBranch;
    private boolean intraBranch;
    private Long documentationPartner;
    private Boolean isReceivingAgentFreeTextAddress;
    private String receivingAgentFreeTextAddress;
    private Boolean isSendingAgentFreeTextAddress;
    private String sendingAgentFreeTextAddress;
    private String placeOfIssue;
    private CarrierDetailResponse carrierDetails;
    private AchievedQuantitiesResponse achievedQuantities;
    private AllocationsResponse allocations;
    private ShipmentWtVolResponse shipmentWtVolResponse;
    private ArrivalDepartureDetailsResponse arrivalDetails;
    private ArrivalDepartureDetailsResponse departureDetails;
    private String sendingAgentCountry;
    private String receivingAgentCountry;
    private PartiesResponse sendingAgent;
    private PartiesResponse receivingAgent;
    private PartiesResponse borrowedFrom;
    private PartiesResponse creditor;
    private PartiesResponse coLoadWith;
    private String createdBy;
    private List<String> houseBills;
    private List<String> shipmentIds;
    private List<ReferenceNumbersResponse> referenceNumbersList;
    private String bookingId;
    private List<PartiesResponse> consolidationAddresses;
    private String bookingStatus;
    private String bookingNumber;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime createdAt;
    private String carrierBookingRef;
    private Map<String, String> masterData;
    private Map<String, String> unlocationData;
    private Map<String, String> currenciesMasterData;
    private Map<String, String> tenantIdsData;
    private Map<String, String> textData;
    private ContainerSummaryResponse containerSummary;
    private PackSummaryResponse packSummary;
    private String modeOfBooking;
    private Boolean autoUpdateGoodsDesc;
    private UUID sourceGuid;
    private String efreightStatus;
    private AwbStatus awbStatus;
    private AwbStatus linkedHawbStatus;
    private Boolean hazardous;
    private Boolean reefer;
    private String emergencyContactNumber;
    private String emergencyContactNumberCode;
    private Boolean creatingFromDgShipment;
    private String securityStatus;
    private List<String> screeningStatus;
    private String exemptionCodes;
    private String aomFreeText;
    private PartiesResponse client;
    private PartiesResponse consigner;
    private PartiesResponse consignee;
    private String sci;
    private String additionalSecurityInformation;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime cfsCutOffDate;
    private Boolean openForAttachment;
    private Boolean interBranchConsole;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime latDate;
    private Integer pendingActionCount;
    private String department;
    private Boolean isNetworkFlag;
    private String transferStatus;
    private Boolean isReceivingBranchManually;
    private Boolean isTransferredToReceivingBranch;
    private String partner;
    private Long bookingAgent;
    private String coLoadCarrierName;
    private Boolean borrowed;
    private Long originBranch;
    private Long shipmentsCount;
    private Boolean isMainCarriageAvailable = Boolean.FALSE;
    private String incoterms;
    private TransportInfoStatus transportInfoStatus;
    private String transportInfoStatusMessage;
    private Boolean isVesselVoyageOrCarrierFlightNumberAvailable = Boolean.FALSE;
    private MigrationStatus migrationStatus;
    private Boolean triggerMigrationWarning;
    private Map<String, Object> masterDataMap;
    private Boolean controlled;
    private String controlledReferenceNumber;
}
