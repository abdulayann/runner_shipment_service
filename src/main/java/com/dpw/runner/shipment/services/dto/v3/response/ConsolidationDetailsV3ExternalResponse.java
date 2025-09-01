package com.dpw.runner.shipment.services.dto.v3.response;

import com.dpw.runner.shipment.services.commons.enums.TransportInfoStatus;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentWtVolResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.enums.AwbStatus;
import com.dpw.runner.shipment.services.entity.enums.MigrationStatus;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.*;

import java.util.Map;
import java.time.LocalDateTime;
import java.util.UUID;
import java.util.List;


@Builder
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@Data
public class ConsolidationDetailsV3ExternalResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private String consolidationType;
    private String consolidationNumber;
    private Integer tenantId;
    private String transportMode;
    private String assignedTo;
    private String mawb;
    private String payment;
    private String containerCategory;
    private String deliveryMode;
    private String referenceNumber;
    private String coLoadMBL;
    private LocalDateTime terminalCutoff;
    private String dgClass;
    private String coLoadBookingReference;
    private LocalDateTime reeferCutoff;
    private String dgSubstance;
    private LocalDateTime verifiedGrossMassCutoff;
    private LocalDateTime hazardousBookingCutoff;
    private LocalDateTime latestFullEquDeliveredToCarrier;
    private LocalDateTime earliestEmptyEquPickUp;
    private LocalDateTime earliestDropOffFullEquToCarrier;
    private LocalDateTime shipInstructionCutoff;
    private Boolean isLocked;
    private String shipmentType;
    private String lockedBy;
    private String bol;
    private LocalDateTime igmInwardDate;
    private String description;
    private String additionalTerms;
    private LocalDateTime igmFileDate;
    private LocalDateTime inwardDateAndTime;
    private String marksnNums;
    private String igmFileNo;
    private LocalDateTime smtpigmDate;
    private String smtpigmNumber;
    private Boolean isInland;
    private Long receivingBranch;
    private List<TriangulationPartnerResponse> triangulationPartnerList;
    private AchievedQuantitiesResponse achievedQuantities;
    private Long sourceTenantId;
    private CarrierDetailResponse carrierDetails;
    private ShipmentWtVolResponse shipmentWtVolResponse;
    private AllocationsResponse allocations;
    private String sendingAgentCountry;
    private String receivingAgentCountry;
    private PartiesExternalResponse sendingAgent;
    private PartiesExternalResponse borrowedFrom;
    private String createdBy;
    private String bookingNumber;
    private PartiesExternalResponse receivingAgent;
    private List<String> shipmentIds;
    private List<ReferenceNumbersResponse> referenceNumbersList;
    private String bookingId;
    private List<PartiesExternalResponse> consolidationAddresses;
    private List<String> houseBills;
    private String bookingStatus;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime createdAt;
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
    private Boolean hazardous;
    private String emergencyContactNumber;
    private List<String> screeningStatus;
    private String aomFreeText;
    private String securityStatus;
    private String exemptionCodes;
    private PartiesExternalResponse consigner;
    private PartiesExternalResponse consignee;
    private String additionalSecurityInformation;
    private PartiesExternalResponse client;
    private String sci;
    private Boolean openForAttachment;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime latDate;
    private Boolean interBranchConsole;
    private String department;
    private Integer pendingActionCount;
    private String transferStatus;
    private Boolean isNetworkFlag;
    private Boolean isTransferredToReceivingBranch;
    private Boolean isReceivingBranchManually;
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
    private List<Routings> routingsList;
    private List<Packing> packingList;
    private List<Events> eventsList;
    private List<Containers> containersList;
    private List<TruckDriverDetailsResponse> truckDriverDetails;
}
