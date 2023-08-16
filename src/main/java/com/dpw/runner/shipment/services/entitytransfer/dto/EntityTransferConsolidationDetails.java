package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import lombok.*;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class EntityTransferConsolidationDetails implements IEntityTranferBaseEntity {
    private String consolidationNumber;
    private String consolidationType;
    private String transportMode;
    private String containerCategory;
    private Boolean isDomestic;
    private String MAWB;
    private String serviceLevel;
    private String payment;
    private String firstLoad;
    private String lastDischarge;
    private String bookingType;
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
    private Boolean override;
    private LocalDateTime EstimatedTerminalCutoff;
    private LocalDateTime TerminalCutoff;
    private LocalDateTime VerifiedGrossMassCutoff;
    private LocalDateTime ReeferCutoff;
    private LocalDateTime BookingCutoff;
    private LocalDateTime ShipInstructionCutoff;
    private LocalDateTime HazardousBookingCutoff;
    private String volumeUtilization;
    private String weightUtilization;
    private String shipmentType;
    private String bol;
    private Boolean isCargoOnly;
    private Boolean isLocked;
    private String lockedBy;
    private String specialInstructions;
    private String description;
    private String MarksnNums;
    private String AdditionalTerms;
    private LocalDateTime docsClosingTime;
    private LocalDateTime cargoClosingTime;
    private String mrnNumber;
    private String msnNumber;
    private LocalDateTime IGMFileDate;
    private LocalDateTime IGMInwardDate;
    private LocalDateTime inwardDateAndTime;
    private String igmFileNo;
    private String SMTPIGMNumber;
    private LocalDateTime SMTPIGMDate;
    private Boolean isInland;
    private Integer original;
    private Integer copy;
    private String DOPlaceOfIssue;
    private LocalDateTime DOIssueDate;
    private Long bondedWarehouseId;
    private Long warehouseId;
    private long SourceTenantId;
    private String ediTransactionId;
    private long triangulationPartner;
    private long receivingBranch;
    private boolean intraBranch;
    private long documentationPartner;
    private Boolean isReceivingAgentFreeTextAddress;
    private String receivingAgentFreeTextAddress;
    private Boolean IsSendingAgentFreeTextAddress;
    private String sendingAgentFreeTextAddress;
    private String placeOfIssue;
    private EntityTransferCarrierDetails carrierDetails;
    private EntityTransferAchievedQuantities achievedQuantities;
    private EntityTransferAllocations allocations;
    private EntityTransferArrivalDepartureDetails arrivalDepartureDetails;
    private List<EntityTransferPacking> packingList;
    private List<EntityTransferReferenceNumbers> referenceNumbersList;
    private List<EntityTransferRoutings> routingsList;
    private List<EntityTransferContainers> containersList;
    private List<EntityTransferFileRepo> fileRepoList;
    private List<EntityTransferShipmentDetails> shipmentsList;
    private List<EntityTransferParties> consolidationAddresses;
    private Map<String, EntityTransferMasterLists> masterData;
    private Map<String, EntityTransferUnLocations> unlocationData;
    private Map<UUID, List<UUID>> containerVsShipmentGuid;
}
