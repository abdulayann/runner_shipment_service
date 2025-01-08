package com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel;

import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.TriangulationPartnerModel;
import com.dpw.runner.shipment.services.config.LocalDateTimeWithTimeZoneSerializer;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import java.time.LocalDateTime;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class ConsolidationModel implements IDocumentModel {
    @JsonProperty("Id")
    private Long id;
    @JsonProperty("ConsolidationNumber")
    private String consolidationNumber;
    @JsonProperty("ConsolidationType")
    private String consolidationType;
    @JsonProperty("TransportMode")
    private String transportMode;
    @JsonProperty("ContainerCategory")
    private String containerCategory;
    @JsonProperty("IsDomestic")
    private Boolean isDomestic;
    @JsonProperty("Mawb")
    private String mawb;
    @JsonProperty("ServiceLevel")
    private String serviceLevel;
    @JsonProperty("Payment")
    private String payment;
    @JsonProperty("FirstLoad")
    private String firstLoad;
    @JsonProperty("LastDischarge")
    private String lastDischarge;
    @JsonProperty("BookingType")
    private String bookingType;
    @JsonProperty("DeclarationType")
    private String declarationType;
    @JsonProperty("DeliveryMode")
    private String deliveryMode;
    @JsonProperty("IsLinked")
    private Boolean isLinked;
    @JsonProperty("IsCharter")
    private Boolean isCharter;
    @JsonProperty("ReferenceNumber")
    private String referenceNumber;
    @JsonProperty("PackageType")
    private String packageType;
    @JsonProperty("AgentReference")
    private String agentReference;
    @JsonProperty("CoLoadMBL")
    private String coLoadMBL;
    @JsonProperty("CoLoadBookingReference")
    private String coLoadBookingReference;
    @JsonProperty("ManifestPrint")
    private String manifestPrint;
    @JsonProperty("PrintOtherDocs")
    private String printOtherDocs;
    @JsonProperty("AwbDims")
    private String awbDims;
    @JsonProperty("ReleaseType")
    private String releaseType;
    @JsonProperty("MasterBillIssueDate")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime masterBillIssueDate;
    @JsonProperty("DgClass")
    private String dgClass;
    @JsonProperty("DgSubstance")
    private String dgSubstance;
    @JsonProperty("Override")
    private Boolean override;
    @JsonProperty("EstimatedTerminalCutoff")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime estimatedTerminalCutoff;
    @JsonProperty("TerminalCutoff")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime terminalCutoff;
    @JsonProperty("VerifiedGrossMassCutoff")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime verifiedGrossMassCutoff;
    @JsonProperty("ReeferCutoff")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime reeferCutoff;
    @JsonProperty("BookingCutoff")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime bookingCutoff;
    @JsonProperty("ShipInstructionCutoff")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime shipInstructionCutoff;
    @JsonProperty("HazardousBookingCutoff")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime hazardousBookingCutoff;
    @JsonProperty("VolumeUtilization")
    private String volumeUtilization;
    @JsonProperty("WeightUtilization")
    private String weightUtilization;
    @JsonProperty("ShipmentType")
    private String shipmentType;
    @JsonProperty("Bol")
    private String bol;
    @JsonProperty("IsCargoOnly")
    private Boolean isCargoOnly;
    @JsonProperty("IsLocked")
    private Boolean isLocked;
    @JsonProperty("LockedBy")
    private String lockedBy;
    @JsonProperty("SpecialInstructions")
    private String specialInstructions;
    @JsonProperty("Description")
    private String description;
    @JsonProperty("MarksnNums")
    private String marksnNums;
    @JsonProperty("AdditionalTerms")
    private String additionalTerms;
    @JsonProperty("DocsClosingTime")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime docsClosingTime;
    @JsonProperty("CargoClosingTime")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime cargoClosingTime;
    @JsonProperty("MrnNumber")
    private String mrnNumber;
    @JsonProperty("MsnNumber")
    private String msnNumber;
    @JsonProperty("IgmFileDate")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime igmFileDate;
    @JsonProperty("IgmInwardDate")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime igmInwardDate;
    @JsonProperty("InwardDateAndTime")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime inwardDateAndTime;
    @JsonProperty("IgmFileNo")
    private String igmFileNo;
    @JsonProperty("SmtpigmNumber")
    private String smtpigmNumber;
    @JsonProperty("SmtpigmDate")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime smtpigmDate;
    @JsonProperty("IsInland")
    private Boolean isInland;
    @JsonProperty("Original")
    private Integer original;
    @JsonProperty("Copy")
    private Integer copy;
    @JsonProperty("DoPlaceOfIssue")
    private String doPlaceOfIssue;
    @JsonProperty("DoIssueDate")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime doIssueDate;
    @JsonProperty("BondedWarehouseId")
    private Long bondedWarehouseId;
    @JsonProperty("WarehouseId")
    private Long warehouseId;
    @JsonProperty("SourceTenantId")
    private long sourceTenantId;
    @JsonProperty("EdiTransactionId")
    private String ediTransactionId;
    @JsonProperty("TriangulationPartnerList")
    private List<TriangulationPartnerModel> triangulationPartnerList;
    @JsonProperty("TriangulationPartner")
    private long triangulationPartner;
    @JsonProperty("ReceivingBranch")
    private long receivingBranch;
    @JsonProperty("IntraBranch")
    private boolean intraBranch;
    @JsonProperty("DocumentationPartner")
    private long documentationPartner;
    @JsonProperty("IsReceivingAgentFreeTextAddress")
    private Boolean isReceivingAgentFreeTextAddress;
    @JsonProperty("ReceivingAgentFreeTextAddress")
    private String receivingAgentFreeTextAddress;
    @JsonProperty("IsSendingAgentFreeTextAddress")
    private Boolean isSendingAgentFreeTextAddress;
    @JsonProperty("SendingAgentFreeTextAddress")
    private String sendingAgentFreeTextAddress;
    @JsonProperty("PlaceOfIssue")
    private String placeOfIssue;
    @JsonProperty("CarrierDetails")
    private CarrierDetailModel carrierDetails;
    @JsonProperty("AchievedQuantities")
    private AchievedQuantitiesModel achievedQuantities;
    @JsonProperty("Allocations")
    private AllocationsModel allocations;
    @JsonProperty("ArrivalDetails")
    private ArrivalDepartureDetailsModel arrivalDetails;
    @JsonProperty("DepartureDetails")
    private ArrivalDepartureDetailsModel departureDetails;
    @JsonProperty("SendingAgent")
    private PartiesModel sendingAgent;
    @JsonProperty("ReceivingAgent")
    private PartiesModel receivingAgent;
    @JsonProperty("BorrowedFrom")
    private PartiesModel borrowedFrom;
    @JsonProperty("Creditor")
    private PartiesModel creditor;
    @JsonProperty("CoLoadWith")
    private PartiesModel coLoadWith;
    @JsonProperty("PackingList")
    private List<PackingModel> packingList;
    @JsonProperty("ReferenceNumbersList")
    private List<ReferenceNumbersModel> referenceNumbersList;
    @JsonProperty("RoutingsList")
    private List<RoutingsModel> routingsList;
    @JsonProperty("ContainersList")
    private List<ContainerModel> containersList;
    @JsonProperty("TruckDriverDetails")
    private List<TruckDriverDetailsModel> truckDriverDetails;
    @JsonProperty("JobsList")
    private List<JobModel> jobsList;
    @JsonProperty("EventsList")
    private List<EventsModel> eventsList;
    @JsonProperty("FileRepoList")
    private List<FileRepoModel> fileRepoList;
    @JsonProperty("ShipmentsList")
    @JsonIgnoreProperties("ConsolidationList")
    private List<ShipmentModel> shipmentsList;
    @JsonProperty("ShipmentIds")
    private List<Long> shipmentIds;
    @JsonProperty("ConsolidationAddresses")
    private List<PartiesModel> consolidationAddresses;
    @JsonProperty("Hazardous")
    private Boolean hazardous;
    @JsonProperty("EmergencyContactNumber")
    private String emergencyContactNumber;
    @JsonProperty("EmergencyContactNumberCode")
    private String emergencyContactNumberCode;
    @JsonProperty("screeningStatus")
    private List<String> screeningStatus;
    @JsonProperty("exemptionCodes")
    private String exemptionCodes;
    @JsonProperty("aomFreeText")
    private String aomFreeText;
    @JsonProperty("securityStatus")
    private String securityStatus;
    @JsonProperty("consoleGrossWeightAndUnit")
    private String consoleGrossWeightAndUnit;
    @JsonProperty("Sci")
    private String sci;
    @JsonProperty("AdditionalSecurityInformation")
    private String additionalSecurityInformation;
}
