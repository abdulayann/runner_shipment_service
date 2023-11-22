package com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class ConsolidationModel {
    @JsonProperty("id")
    private Long id;
    @JsonProperty("consolidationNumber")
    private String consolidationNumber;
    @JsonProperty("consolidationType")
    private String consolidationType;
    @JsonProperty("transportMode")
    private String transportMode;
    @JsonProperty("containerCategory")
    private String containerCategory;
    @JsonProperty("isDomestic")
    private Boolean isDomestic;
    @JsonProperty("mawb")
    private String mawb;
    @JsonProperty("serviceLevel")
    private String serviceLevel;
    @JsonProperty("payment")
    private String payment;
    @JsonProperty("firstLoad")
    private String firstLoad;
    @JsonProperty("lastDischarge")
    private String lastDischarge;
    @JsonProperty("bookingType")
    private String bookingType;
    @JsonProperty("declarationType")
    private String declarationType;
    @JsonProperty("deliveryMode")
    private String deliveryMode;
    @JsonProperty("isLinked")
    private Boolean isLinked;
    @JsonProperty("isCharter")
    private Boolean isCharter;
    @JsonProperty("referenceNumber")
    private String referenceNumber;
    @JsonProperty("packageType")
    private String packageType;
    @JsonProperty("agentReference")
    private String agentReference;
    @JsonProperty("coLoadMBL")
    private String coLoadMBL;
    @JsonProperty("coLoadBookingReference")
    private String coLoadBookingReference;
    @JsonProperty("manifestPrint")
    private String manifestPrint;
    @JsonProperty("printOtherDocs")
    private String printOtherDocs;
    @JsonProperty("awbDims")
    private String awbDims;
    @JsonProperty("releaseType")
    private String releaseType;
    @JsonProperty("masterBillIssueDate")
    private LocalDateTime masterBillIssueDate;
    @JsonProperty("dgClass")
    private String dgClass;
    @JsonProperty("dgSubstance")
    private String dgSubstance;
    @JsonProperty("override")
    private Boolean override;
    @JsonProperty("estimatedTerminalCutoff")
    private LocalDateTime estimatedTerminalCutoff;
    @JsonProperty("terminalCutoff")
    private LocalDateTime terminalCutoff;
    @JsonProperty("verifiedGrossMassCutoff")
    private LocalDateTime verifiedGrossMassCutoff;
    @JsonProperty("reeferCutoff")
    private LocalDateTime reeferCutoff;
    @JsonProperty("bookingCutoff")
    private LocalDateTime bookingCutoff;
    @JsonProperty("shipInstructionCutoff")
    private LocalDateTime shipInstructionCutoff;
    @JsonProperty("hazardousBookingCutoff")
    private LocalDateTime hazardousBookingCutoff;
    @JsonProperty("volumeUtilization")
    private String volumeUtilization;
    @JsonProperty("weightUtilization")
    private String weightUtilization;
    @JsonProperty("shipmentType")
    private String shipmentType;
    @JsonProperty("bol")
    private String bol;
    @JsonProperty("isCargoOnly")
    private Boolean isCargoOnly;
    @JsonProperty("isLocked")
    private Boolean isLocked;
    @JsonProperty("lockedBy")
    private String lockedBy;
    @JsonProperty("specialInstructions")
    private String specialInstructions;
    @JsonProperty("description")
    private String description;
    @JsonProperty("marksnNums")
    private String marksnNums;
    @JsonProperty("additionalTerms")
    private String additionalTerms;
    @JsonProperty("docsClosingTime")
    private LocalDateTime docsClosingTime;
    @JsonProperty("cargoClosingTime")
    private LocalDateTime cargoClosingTime;
    @JsonProperty("mrnNumber")
    private String mrnNumber;
    @JsonProperty("msnNumber")
    private String msnNumber;
    @JsonProperty("igmFileDate")
    private LocalDateTime igmFileDate;
    @JsonProperty("igmInwardDate")
    private LocalDateTime igmInwardDate;
    @JsonProperty("inwardDateAndTime")
    private LocalDateTime inwardDateAndTime;
    @JsonProperty("igmFileNo")
    private String igmFileNo;
    @JsonProperty("smtpigmNumber")
    private String smtpigmNumber;
    @JsonProperty("smtpigmDate")
    private LocalDateTime smtpigmDate;
    @JsonProperty("isInland")
    private Boolean isInland;
    @JsonProperty("original")
    private Integer original;
    @JsonProperty("copy")
    private Integer copy;
    @JsonProperty("doPlaceOfIssue")
    private String doPlaceOfIssue;
    @JsonProperty("doIssueDate")
    private LocalDateTime doIssueDate;
    @JsonProperty("bondedWarehouseId")
    private Long bondedWarehouseId;
    @JsonProperty("warehouseId")
    private Long warehouseId;
    @JsonProperty("sourceTenantId")
    private long sourceTenantId;
    @JsonProperty("ediTransactionId")
    private String ediTransactionId;
    @JsonProperty("triangulationPartner")
    private long triangulationPartner;
    @JsonProperty("receivingBranch")
    private long receivingBranch;
    @JsonProperty("intraBranch")
    private boolean intraBranch;
    @JsonProperty("documentationPartner")
    private long documentationPartner;
    @JsonProperty("isReceivingAgentFreeTextAddress")
    private Boolean isReceivingAgentFreeTextAddress;
    @JsonProperty("receivingAgentFreeTextAddress")
    private String receivingAgentFreeTextAddress;
    @JsonProperty("isSendingAgentFreeTextAddress")
    private Boolean isSendingAgentFreeTextAddress;
    @JsonProperty("sendingAgentFreeTextAddress")
    private String sendingAgentFreeTextAddress;
    @JsonProperty("placeOfIssue")
    private String placeOfIssue;
    @JsonProperty("carrierDetails")
    private CarrierDetailModel carrierDetails;
    @JsonProperty("achievedQuantities")
    private AchievedQuantitiesModel achievedQuantities;
    @JsonProperty("allocations")
    private AllocationsModel allocations;
    @JsonProperty("arrivalDetails")
    private ArrivalDepartureDetailsModel arrivalDetails;
    @JsonProperty("departureDetails")
    private ArrivalDepartureDetailsModel departureDetails;
    @JsonProperty("sendingAgent")
    private PartiesModel sendingAgent;
    @JsonProperty("receivingAgent")
    private PartiesModel receivingAgent;
    @JsonProperty("borrowedFrom")
    private PartiesModel borrowedFrom;
    @JsonProperty("creditor")
    private PartiesModel creditor;
    @JsonProperty("coLoadWith")
    private PartiesModel coLoadWith;
    @JsonProperty("packingList")
    private List<PackingModel> packingList;
    @JsonProperty("referenceNumbersList")
    private List<ReferenceNumbersModel> referenceNumbersList;
    @JsonProperty("routingsList")
    private List<RoutingsModel> routingsList;
    @JsonProperty("containersList")
    private List<ContainerModel> containersList;
    @JsonProperty("truckDriverDetails")
    private List<TruckDriverDetailsModel> truckDriverDetails;
    @JsonProperty("jobsList")
    private List<JobModel> jobsList;
    @JsonProperty("eventsList")
    private List<EventsModel> eventsList;
    @JsonProperty("fileRepoList")
    private List<FileRepoModel> fileRepoList;
    @JsonProperty("shipmentsList")
    @JsonIgnoreProperties("consolidationList")
    private List<ShipmentModel> shipmentsList;
    @JsonProperty("shipmentIds")
    private List<Long> shipmentIds;
    @JsonProperty("consolidationAddresses")
    private List<PartiesModel> consolidationAddresses;
}
