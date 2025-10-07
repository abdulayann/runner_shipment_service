package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.enums.TransportInfoStatus;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
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
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ConsolidationDetailsBaseResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Integer tenantId;
    private String consolidationNumber;
    private String consolidationType;
    private String transportMode;
    private String containerCategory;
    private String assignedTo;
    private String mawb;
    private String payment;
    private String referenceNumber;
    private String coLoadMBL;
    private String coLoadBookingReference;
    private String dgClass;
    private String dgSubstance;
    private LocalDateTime terminalCutoff; //- custom serializer added in V3 response
    private LocalDateTime reeferCutoff; //- custom serializer added in V3 response
    private LocalDateTime verifiedGrossMassCutoff; //- custom serializer added in V3 response
    private LocalDateTime hazardousBookingCutoff; //- custom serializer added in V3 response
    private LocalDateTime shipInstructionCutoff; //- custom serializer added in V3 response
    private Boolean isLocked;
    private String lockedBy;
    private String shipmentType;
    private String bol;
    private String description;
    private String marksnNums;
    private String additionalTerms;
    private LocalDateTime igmFileDate; //- custom serializer added in V3 response
    private LocalDateTime igmInwardDate; //- custom serializer added in V3 response
    private LocalDateTime inwardDateAndTime; //- custom serializer added in V3 response
    private String igmFileNo;
    private String smtpigmNumber;
    private LocalDateTime smtpigmDate; //- custom serializer added in V3 response
    private Boolean isInland;
    private Long receivingBranch;
    private List<TriangulationPartnerResponse> triangulationPartnerList;
    private AchievedQuantitiesResponse achievedQuantities;
    private Long sourceTenantId;
    private CarrierDetailResponse carrierDetails;
    private AllocationsResponse allocations;
    private String createdBy;
    private String bookingId;
    private String bookingStatus;
    private String bookingNumber;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime createdAt; //- custom serializer not added in List Response
    private Boolean hazardous;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime latDate;
    private Integer pendingActionCount;
    private String department;
    private Boolean isNetworkFlag;
    private Boolean isReceivingBranchManually;
    private Boolean isTransferredToReceivingBranch;
    private Boolean controlled;
    private String controlledReferenceNumber;
    private String deliveryMode;
    private ShipmentWtVolResponse shipmentWtVolResponse;
    private String sendingAgentCountry;
    private String receivingAgentCountry;
    private List<String> shipmentIds;
    private List<ReferenceNumbersResponse> referenceNumbersList;
    private List<String> houseBills;
    private Map<String, String> masterData;
    private Map<String, String> currenciesMasterData;
    private String carrierBookingRef;
    private Map<String, String> unlocationData;
    private Map<String, String> textData;
    private String modeOfBooking;
    private Map<String, String> tenantIdsData;
    private Boolean reefer;
    private String efreightStatus;
    private AwbStatus linkedHawbStatus;
    private UUID sourceGuid;
    private AwbStatus awbStatus;
    private String emergencyContactNumberCode;
    private Boolean creatingFromDgShipment;
    private String emergencyContactNumber;
    private List<String> screeningStatus;
    private String aomFreeText;
    private String securityStatus;
    private String exemptionCodes;
    private String additionalSecurityInformation;
    private String sci;
    private Boolean openForAttachment;
    private Boolean interBranchConsole;
    private String transferStatus;
    private String coLoadCarrierName;
    private String partner;
    private Long shipmentsCount;
    private Long bookingAgent;
    private Long originBranch;
    private Boolean isMainCarriageAvailable = Boolean.FALSE;
    private TransportInfoStatus transportInfoStatus;
    private String transportInfoStatusMessage;
    private Boolean borrowed;
    private String incoterms;
    private Boolean triggerMigrationWarning;
    private Boolean isVesselVoyageOrCarrierFlightNumberAvailable = Boolean.FALSE;
    private MigrationStatus migrationStatus;

}
