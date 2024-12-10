package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerSummaryResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.dto.response.TriangulationPartnerResponse;
import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
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
import lombok.ToString;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class EntityTransferConsolidationDetails implements IEntityTranferBaseEntity {
    private UUID guid;
    private String consolidationNumber;
    private String consolidationType;
    private String transportMode;
    private String containerCategory;
    private Boolean isDomestic;
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
    private LocalDateTime masterBillIssueDate;
    private String dgClass;
    private String dgSubstance;
    private Boolean override;
    private LocalDateTime estimatedTerminalCutoff;
    private LocalDateTime terminalCutoff;
    private LocalDateTime verifiedGrossMassCutoff;
    private LocalDateTime reeferCutoff;
    private LocalDateTime bookingCutoff;
    private LocalDateTime shipInstructionCutoff;
    private LocalDateTime hazardousBookingCutoff;
    private LocalDateTime latestFullEquDeliveredToCarrier;
    private LocalDateTime earliestDropOffFullEquToCarrier;
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
    private LocalDateTime docsClosingTime;
    private LocalDateTime cargoClosingTime;
    private String mrnNumber;
    private String msnNumber;
    private LocalDateTime igmFileDate;
    private LocalDateTime igmInwardDate;
    private LocalDateTime inwardDateAndTime;
    private String igmFileNo;
    private String smtpigmNumber;
    private LocalDateTime smtpigmDate;
    private Boolean isInland;
    private Integer original;
    private Integer copy;
    private String doPlaceOfIssue;
    private LocalDateTime doIssueDate;
    private Long bondedWarehouseId;
    private Long warehouseId;
    private Long sourceTenantId;
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


    private EntityTransferCarrierDetails carrierDetails;
    private EntityTransferAchievedQuantities achievedQuantities;
    private EntityTransferAllocations allocations;
    private EntityTransferArrivalDepartureDetails arrivalDetails;
    private EntityTransferArrivalDepartureDetails departureDetails;
    private EntityTransferParties sendingAgent;
    private EntityTransferParties receivingAgent;
    private EntityTransferParties borrowedFrom;
    private EntityTransferParties creditor;
    private EntityTransferParties coLoadWith;
    private List<EntityTransferPacking> packingList;
    private List<EntityTransferReferenceNumbers> referenceNumbersList;
    private List<EntityTransferRoutings> routingsList;
    private List<EntityTransferContainers> containersList;

    private List<String> houseBills;
    private List<String> shipmentIds;
    private String bookingId;
    private List<EntityTransferParties> consolidationAddresses;
    private String bookingStatus;
    private String bookingNumber;
    private String carrierBookingRef;

    private ContainerSummaryResponse containerSummary;
    private PackSummaryResponse packSummary;
    private String modeOfBooking;
    private Boolean autoUpdateGoodsDesc;
    private String efreightStatus;
    private Boolean hazardous;
    private String emergencyContactNumber;
    private String emergencyContactNumberCode;
    private Boolean creatingFromDgShipment;
    private String securityStatus;
    private List<String> screeningStatus;
    private String exemptionCodes;
    private String aomFreeText;
    private EntityTransferParties client;
    private EntityTransferParties consigner;
    private EntityTransferParties consignee;
    private String sci;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime cfsCutOffDate;
    private Boolean openForAttachment;
    private Boolean interBranchConsole;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime latDate;

    private List<EntityTransferShipmentDetails> shipmentsList;

    private Map<UUID, UUID> packingVsContainerGuid;
    private Map<UUID, List<UUID>> containerVsShipmentGuid;

    private Integer sendToBranch;
    private String sourceBranchTenantName;

    private transient Map<String, Object> masterData;

    private List<String> additionalDocs;
    private Map<String, List<String>> shipAdditionalDocs;

}
