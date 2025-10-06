package com.dpw.runner.shipment.services.dto.v3.response;

import com.dpw.runner.shipment.services.commons.enums.TransportInfoStatus;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerSummaryResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentWtVolResponse;
import com.dpw.runner.shipment.services.entity.enums.AwbStatus;
import com.dpw.runner.shipment.services.entity.enums.MigrationStatus;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@Data
public class ConsolidationDetailsV3Response extends ConsolidationDetailsBaseResponse implements IRunnerResponse {
    private Boolean isDomestic;
    private String serviceLevel;
    private String declarationType;
    private String deliveryMode;
    private Boolean isLinked;
    private Boolean isCharter;
    private String packageType;
    private String agentReference;
    private String manifestPrint;
    private String printOtherDocs;
    private String awbDims;
    private String releaseType;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime masterBillIssueDate;
    private Boolean override;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime estimatedTerminalCutoff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime terminalCutoff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime verifiedGrossMassCutoff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime reeferCutoff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime bookingCutoff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime shipInstructionCutoff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime hazardousBookingCutoff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime latestFullEquDeliveredToCarrier;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime earliestDropOffFullEquToCarrier;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime earliestEmptyEquPickUp;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime carrierDocCutOff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime cargoReceiptWHCutOff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime lastFreeDateCutOff;
    private Integer numberOfFreeDaysCutOff;
    private String volumeUtilization;
    private String weightUtilization;
    private Boolean isCargoOnly;
    private String specialInstructions;
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
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime smtpigmDate;
    private Integer original;
    private Integer copy;
    private String doPlaceOfIssue;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime doIssueDate;
    private Long bondedWarehouseId;
    private Long warehouseId;
    private String ediTransactionId;
    private Long triangulationPartner;
    private boolean intraBranch;
    private Long documentationPartner;
    private Boolean isReceivingAgentFreeTextAddress;
    private String receivingAgentFreeTextAddress;
    private Boolean isSendingAgentFreeTextAddress;
    private String sendingAgentFreeTextAddress;
    private String placeOfIssue;
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
    private List<String> houseBills;
    private List<String> shipmentIds;
    private List<ReferenceNumbersResponse> referenceNumbersList;
    private List<PartiesResponse> consolidationAddresses;
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
    private String transferStatus;
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
}
