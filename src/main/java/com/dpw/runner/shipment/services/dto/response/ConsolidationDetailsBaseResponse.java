package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.time.LocalDateTime;
import java.util.List;
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

}
