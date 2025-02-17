package com.dpw.runner.shipment.services.dto.patchrequest;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;

import java.time.LocalDateTime;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.openapitools.jackson.nullable.JsonNullable;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ConsolidationPatchRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private JsonNullable<String> consolidationNumber;
    private JsonNullable<String> consolidationType;
    private JsonNullable<String> transportMode;
    private JsonNullable<String> containerCategory;
    private JsonNullable<Boolean> isDomestic;
    private JsonNullable<String> mawb;
    private JsonNullable<String> serviceLevel;
    private JsonNullable<String> payment;
    private JsonNullable<String> firstLoad;
    private JsonNullable<String> lastDischarge;
    private JsonNullable<String> declarationType;
    private JsonNullable<String> deliveryMode;
    private JsonNullable<Boolean> isLinked;
    private JsonNullable<Boolean> isCharter;
    private JsonNullable<String> referenceNumber;
    private JsonNullable<String> packageType;
    private JsonNullable<String> agentReference;
    private JsonNullable<String> coLoadMBL;
    private JsonNullable<String> coLoadBookingReference;
    private JsonNullable<String> manifestPrint;
    private JsonNullable<String> printOtherDocs;
    private JsonNullable<String> awbDims;
    private JsonNullable<String> releaseType;
    @ExcludeTimeZone
    private JsonNullable<LocalDateTime> masterBillIssueDate;
    private JsonNullable<String> dgClass;
    private JsonNullable<String> dgSubstance;
    private JsonNullable<Boolean> override;
    @ExcludeTimeZone
    private JsonNullable<LocalDateTime> estimatedTerminalCutoff;
    @ExcludeTimeZone
    private JsonNullable<LocalDateTime> terminalCutoff;
    @ExcludeTimeZone
    private JsonNullable<LocalDateTime> verifiedGrossMassCutoff;
    @ExcludeTimeZone
    private JsonNullable<LocalDateTime> reeferCutoff;
    @ExcludeTimeZone
    private JsonNullable<LocalDateTime> bookingCutoff;
    @ExcludeTimeZone
    private JsonNullable<LocalDateTime> shipInstructionCutoff;
    @ExcludeTimeZone
    private JsonNullable<LocalDateTime> hazardousBookingCutoff;
    private JsonNullable<String> volumeUtilization;
    private JsonNullable<String> weightUtilization;
    private JsonNullable<String> shipmentType;
    private JsonNullable<String> bol;
    private JsonNullable<Boolean> isCargoOnly;
    private JsonNullable<Boolean> isLocked;
    private JsonNullable<String> lockedBy;
    private JsonNullable<String> specialInstructions;
    private JsonNullable<String> description;
    private JsonNullable<String> marksnNums;
    private JsonNullable<String> additionalTerms;
    @ExcludeTimeZone
    private JsonNullable<LocalDateTime> docsClosingTime;
    @ExcludeTimeZone
    private JsonNullable<LocalDateTime> cargoClosingTime;
    private JsonNullable<String> mrnNumber;
    private JsonNullable<String> msnNumber;
    @ExcludeTimeZone
    private JsonNullable<LocalDateTime> igmFileDate;
    @ExcludeTimeZone
    private JsonNullable<LocalDateTime> igmInwardDate;
    @ExcludeTimeZone
    private JsonNullable<LocalDateTime> inwardDateAndTime;
    private JsonNullable<String> igmFileNo;
    private JsonNullable<String> smtpigmNumber;
    @ExcludeTimeZone
    private JsonNullable<LocalDateTime> smtpigmDate;
    private JsonNullable<Boolean> isInland;
    private JsonNullable<Integer> original;
    private JsonNullable<Integer> copy;
    private JsonNullable<String> doPlaceOfIssue;
    @ExcludeTimeZone
    private JsonNullable<LocalDateTime> doIssueDate;
    private JsonNullable<Long> bondedWarehouseId;
    private JsonNullable<Long> warehouseId;
    private JsonNullable<Long> sourceTenantId;
    private JsonNullable<String> ediTransactionId;
    private JsonNullable<List<TriangulationPartnerRequest>> triangulationPartnerList;
    private JsonNullable<Long> triangulationPartner;
    private JsonNullable<Long> receivingBranch;
    private JsonNullable<Boolean> intraBranch;
    private JsonNullable<Long> documentationPartner;
    private JsonNullable<Boolean> isReceivingAgentFreeTextAddress;
    private JsonNullable<String> receivingAgentFreeTextAddress;
    private JsonNullable<Boolean> isSendingAgentFreeTextAddress;
    private JsonNullable<String> sendingAgentFreeTextAddress;
    private JsonNullable<String> placeOfIssue;
    private CarrierPatchRequest carrierDetails;
    private JsonNullable<AchievedQuantitiesRequest> achievedQuantities;
    private JsonNullable<AllocationsRequest> allocations;
    private JsonNullable<ArrivalDepartureDetailsRequest> arrivalDetails;
    private JsonNullable<ArrivalDepartureDetailsRequest> departureDetails;
    private JsonNullable<PartiesRequest> sendingAgent;
    private JsonNullable<PartiesRequest> receivingAgent;
    private JsonNullable<PartiesRequest> borrowedFrom;
    private JsonNullable<PartiesRequest> creditor;
    private JsonNullable<PartiesRequest> coLoadWith;
    private List<PackingRequest> packingList;
    private List<ReferenceNumbersRequest> referenceNumbersList;
    private List<RoutingsRequest> routingsList;
    private List<ContainerRequest> containersList;
    private List<TruckDriverDetailsRequest> truckDriverDetails;
    private List<JobRequest> jobsList;
    private List<EventsRequest> eventsList;
    private List<FileRepoRequest> fileRepoList;
    private List<ShipmentRequest> shipmentsList;
    private List<Long> shipmentIds;
    private List<PartiesRequest> consolidationAddresses;
    private JsonNullable<Boolean> openForAttachment;
    private JsonNullable<Boolean> isNetworkFile;
    private JsonNullable<Boolean> isReceivingBranchManually;
    private JsonNullable<Boolean> isTransferredToReceivingBranch;
}
