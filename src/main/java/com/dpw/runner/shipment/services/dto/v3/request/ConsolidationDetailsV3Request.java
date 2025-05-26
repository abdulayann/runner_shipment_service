package com.dpw.runner.shipment.services.dto.v3.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import com.dpw.runner.shipment.services.utils.TrimStringDeserializer;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.Size;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ConsolidationDetailsV3Request extends CommonRequest implements IRunnerRequest {
    private Long id;
    @Size(max=20, message = "Max size is 20 for Consolidation Number")
    private String consolidationNumber;
    @Size(max=100, message = "Max size is 100 for consolidation_type")
    private String consolidationType;
    @Size(max=3, message = "Max size is 3 for Transport Mode")
    private String transportMode;
    @Size(max=100, message = "max size is 100 for container_category")
    private String containerCategory;
    private Boolean isDomestic;
    @JsonDeserialize(using = TrimStringDeserializer.class)
    private String mawb;
    @Size(max=20, message = "max size is 20 for service_level")
    private String serviceLevel;
    @Size(max=3, message = "Max size is 3 for Payment")
    private String payment;
    private String declarationType;
    private String deliveryMode;
    private Boolean isLinked;
    private Boolean isCharter;
    private String referenceNumber;
    private String packageType;
    @Size(max=64, message = "max size is 64 for agent_reference")
    private String agentReference;
    @Size(max=64, message = "max size is 64 for co_load_mbl")
    private String coLoadMBL;
    @Size(max=64, message = "max size is 64 for co_load_booking_reference")
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
    @Size(max=3, message = "Max size is 3 for Shipment Type")
    private String shipmentType;
    @Size(max=20, message = "max size is 20 for bol")
    @JsonDeserialize(using = TrimStringDeserializer.class)
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
    @Size(max=50, message = "max size is 50 for mrn_number")
    private String mrnNumber;
    private String msnNumber;
    private LocalDateTime igmFileDate;
    private LocalDateTime igmInwardDate;
    private LocalDateTime inwardDateAndTime;
    @Size(max=10, message = "max size is 10 for igm_file_no")
    private String igmFileNo;
    @Size(max=10, message = "max size is 10 smtp_igm_number")
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
    @Size(max=50, message = "max size is 50 for edi_transaction_id")
    private String ediTransactionId;
    private List<TriangulationPartnerRequest> triangulationPartnerList;
    private Long triangulationPartner;
    private Long receivingBranch;
    private boolean intraBranch;
    private Long documentationPartner;
    private Boolean isReceivingAgentFreeTextAddress;
    private String receivingAgentFreeTextAddress;
    private Boolean isSendingAgentFreeTextAddress;
    private String sendingAgentFreeTextAddress;
    private String placeOfIssue;
    private CarrierDetailRequest carrierDetails;
    private AchievedQuantitiesRequest achievedQuantities;
    private AllocationsRequest allocations;
    private ArrivalDepartureDetailsRequest arrivalDetails;
    private ArrivalDepartureDetailsRequest departureDetails;
    private String sendingAgentCountry;
    private String receivingAgentCountry;
    private PartiesRequest sendingAgent;
    private PartiesRequest receivingAgent;
    private PartiesRequest borrowedFrom;
    private PartiesRequest creditor;
    private PartiesRequest coLoadWith;
    private Long bookingAgent;
    @Size(max = 64, message = "max size is 64 for coload carrier name")
    private String coLoadCarrierName;
    private List<ReferenceNumbersRequest> referenceNumbersList;
    private List<PartiesRequest> consolidationAddresses;
    @Size(max=64, message = "max size is 64 for carrier_booking_ref")
    private String carrierBookingRef;
    @Size(max = 64, message = "max size is 64 for mode_of_booking")
    private String modeOfBooking;
    private Boolean autoUpdateGoodsDesc;
    private UUID sourceGuid;
    @Size(max=3, message = "max size is 3 for efreight_status")
    private String efreightStatus;
    private Boolean hazardous;
    @Size(max=31, message = "max size is 31 for emergency_contact_number")
    private String emergencyContactNumber;
    @Size(max=31, message = "max size is 31 for emergency_contact_number_code")
    private String emergencyContactNumberCode;
    private Boolean creatingFromDgShipment;
    private String securityStatus;
    @Size(max=50, message = "max size is 50 for screening_status")
    private List<String> screeningStatus;
    private String exemptionCodes;
    private String aomFreeText;
    private String sci;
    private String additionalSecurityInformation;
    private LocalDateTime cfsCutOffDate;
    private Boolean openForAttachment;
    private Boolean interBranchConsole;
    @ExcludeTimeZone
    private LocalDateTime latDate;
    @Size(max=32, message = "max size is 32 for department")
    private String department;
    private Boolean isNetworkFile;
    private Boolean isReceivingBranchManually;
    private Boolean isTransferredToReceivingBranch;
    @Size(max = 64, message = "max size is 64 for partner")
    private String partner;
    private Boolean borrowed;
    private Long originBranch;
}
