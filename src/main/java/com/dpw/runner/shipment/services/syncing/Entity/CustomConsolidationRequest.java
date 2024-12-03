package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import lombok.Data;

@Data
public class CustomConsolidationRequest implements IRunnerResponse, IRunnerRequest {

    @JsonProperty("AchievedQuantities")
    private AchievedQuantities AchievedQuantities;
    @JsonProperty("AdditionalTerms")
    private String AdditionalTerms;
    @JsonProperty("Allocations")
    private Allocations Allocations;
    @JsonProperty("ArrivalDepartureDetails")
    private ArrivalDepartureDetails ArrivalDepartureDetails;

    // @JsonProperty("Copy
    // private Integer Copy;

    // Carrier details
    @JsonProperty("DestinationName")
    private String DestinationName;
    @JsonProperty("DestinationPortName")
    private String DestinationPortName;
    @JsonProperty("OriginPortName")
    private String OriginPortName;
    @JsonProperty("OriginName")
    private String OriginName;
    @JsonProperty("ShipmentId")
    private String ShipmentId;
    @JsonProperty("ShippingLine")
    private String ShippingLine;
    @JsonProperty("Voyage")
    private String Voyage;

    @JsonProperty("ContainersList")
    private List<ContainerRequestV2> ContainersList;

    @JsonProperty("BondedWarehouseId")
    private Long BondedWarehouseId;

    @JsonProperty("CargoClosingTime")
    private LocalDateTime CargoClosingTime;

    @JsonProperty("ConsolidatedVolume")
    private BigDecimal ConsolidatedVolume;
    @JsonProperty("DocsClosingTime")
    private LocalDateTime DocsClosingTime;
    @JsonProperty("DocumentationPartner")
    private Integer DocumentationPartner;
    @JsonProperty("DOIssueDate")
    private LocalDateTime DOIssueDate;
    @JsonProperty("DOPlaceOfIssueName")
    private String DOPlaceOfIssueName;
    @JsonProperty("EDITransactionId")
    private String EDITransactionId;
    // @JsonProperty("EstimatedTerminalCutoff
    // private LocalDateTime EstimatedTerminalCutoff;

    @JsonProperty("Guid")
    private UUID Guid;
    @JsonProperty("SourceGuid")
    private UUID SourceGuid;
//    @JsonProperty("Id")
//    private Long Id;
    @JsonProperty("IntraBranch")
    private boolean IntraBranch;
    @JsonProperty("InwardDateandTime")
    private LocalDateTime InwardDateandTime;

    @JsonProperty("IsInland")
    private boolean IsInland;
    @JsonProperty("IsReceivingAgentFreeTextAddress")
    private Boolean IsReceivingAgentFreeTextAddress;
    @JsonProperty("IsSendingAgentFreeTextAddress")
    private Boolean IsSendingAgentFreeTextAddress;
    @JsonProperty("IsCreditorFreeTextAddress")
    private Boolean IsCreditorFreeTextAddress;


    // Using this field to address diff in v1 and v2
    // v1 uses user ID and v2 uses username for lockedBy
    @JsonProperty("LockedByUser")
    private String LockedByUser;

    @JsonProperty("MarksnNums")
    private String MarksnNums;
    @JsonProperty("MrnNumber")
    private String MrnNumber;
    @JsonProperty("MsnNumberStr")
    private String MsnNumberStr;

    @JsonProperty("PackageType")
    private String PackageType;

    @JsonProperty("ReceivingAgentFreeTextAddress")
    private String ReceivingAgentFreeTextAddress;
    @JsonProperty("ReceivingBranch")
    private Integer ReceivingBranch;
    @JsonProperty("ReferenceNumber")
    private String ReferenceNumber;

    @JsonProperty("SendingAgentFreeTextAddress")
    private String SendingAgentFreeTextAddress;
    @JsonProperty("CreditorFreeTextAddress")
    private String CreditorFreeTextAddress;
    @JsonProperty("Smtpigmdate")
    private LocalDateTime Smtpigmdate;
    @JsonProperty("Smtpigmnumber")
    private String Smtpigmnumber;
    @JsonProperty("SourceTenantId")
    private Integer SourceTenantId;
    @JsonProperty("SpecialInstructions")
    private String SpecialInstructions;
    @JsonProperty("TriangulationPartnerList")
    private List<Long> TriangulationPartnerList;
    @JsonProperty("TriangulationPartner")
    private Integer TriangulationPartner;
    @JsonProperty("WarehouseId")
    private Long WarehouseId;

    @JsonProperty("JobsList")
    private List<JobRequestV2> JobsList;
    @JsonProperty("NotesList")
    private List<NoteRequestV2> NotesList;
    @JsonProperty("BookingCarriagesListE")
    private List<BookingCarriageRequestV2> BookingCarriagesListE;
    @JsonProperty("ElDetailsList")
    private List<ElDetailsRequestV2> ElDetailsList;
    @JsonProperty("EventsList")
    private List<EventsRequestV2> EventsList;
    @JsonProperty("DocsList")
    private List<FileRepoRequestV2> DocsList;
    @JsonProperty("PackingList")
    private List<PackingRequestV2> PackingList;
    @JsonProperty("ReferenceNumbersList")
    private List<ReferenceNumbersRequestV2> ReferenceNumbersList;
    @JsonProperty("RoutingsList")
    private List<RoutingsRequestV2> RoutingsList;
    @JsonProperty("ShipmentList")
    private List<CustomShipmentSyncRequest> ShipmentList;
    @JsonProperty("TruckDriverDetail")
    private List<TruckDriverDetailsRequestV2> TruckDriverDetail;

    @JsonProperty("FirstLoadString")
    private String FirstLoadString;
    @JsonProperty("LastDischargeString")
    private String LastDischargeString;
    @JsonProperty("IGMFileNo")
    private String IGMFileNo;
    @JsonProperty("IGMFileDate")
    private LocalDateTime IGMFileDate;
    @JsonProperty("IGMInwardDate")
    private LocalDateTime IGMInwardDate;
    @JsonProperty("SMTPIGMDate")
    private LocalDateTime SMTPIGMDateCaps;
    @JsonProperty("SMTPIGMNumber")
    private String SMTPIGMNumberCaps;

    @JsonProperty("ShipmentGuids")
    private Map<UUID, Integer> shipmentGuids;

    @JsonProperty("SendingAgent")
    private PartyRequestV2 SendingAgent;
    @JsonProperty("ReceivingAgent")
    private PartyRequestV2 ReceivingAgent;
    @JsonProperty("BorrowedFrom")
    private PartyRequestV2 BorrowedFrom;
    @JsonProperty("Creditor")
    private PartyRequestV2 Creditor;
    @JsonProperty("CoLoadWith")
    private PartyRequestV2 CoLoadWith;

    @JsonProperty("Description")
    private String Description;
    @JsonProperty("PlaceOfIssueString")
    private String PlaceOfIssueString;

    ///////// Original Consol Request


    @JsonProperty("shipmentRefNumbers")
    private List<String> shipmentRefNumbers;
    @JsonProperty("Type")
    private String Type;
   @JsonProperty("ConsolidationNumber")
    private String ConsolidationNumber;
    // @JsonProperty("ConsolidationId
    // private Long ConsolidationId;
    @JsonProperty("TransportMode")
    private String TransportMode;
    @JsonProperty("DeliveryMode")
    private String DeliveryMode;
    @JsonProperty("ContainerType")
    private String ContainerType;
    @JsonProperty("FirstLoad")
    private String FirstLoad;
    @JsonProperty("LastDischarge")
    private String LastDischarge;
    @JsonProperty("POLId")
    private String POLId;
    @JsonProperty("PODId")
    private String PODId;
    @JsonProperty("Eta")
    private LocalDateTime Eta;
    @JsonProperty("Ata")
    private LocalDateTime Ata;
    @JsonProperty("Etd")
    private LocalDateTime Etd;
    @JsonProperty("Atd")
    private LocalDateTime Atd;
    @JsonProperty("IsDomestic")
    private boolean IsDomestic;
    @JsonProperty("VoyageNumber")
    private String VoyageNumber;
    @JsonProperty("Vessel")
    private String Vessel;
    @JsonProperty("FlightNumber")
    private String FlightNumber;
    @JsonProperty("AircraftType")
    private String AircraftType;
    @JsonProperty("AircraftRegistration")
    private String AircraftRegistration;
    @JsonProperty("TruckRefNumber")
    private String TruckRefNumber;
    @JsonProperty("OtherInfo")
    private String OtherInfo;
    @JsonProperty("JourneyNumber")
    private String JourneyNumber;
    @JsonProperty("JourneyRefNumber")
    private String JourneyRefNumber;
    @JsonProperty("Bol")
    private String Bol;
    @JsonProperty("MAWB")
    private String MAWB;
    @JsonProperty("ServiceLevel")
    private String ServiceLevel;
    @JsonProperty("Payment")
    private String Payment;
//    @JsonProperty("ConsolidationContainers
//    private List<ConsolidationContainersRow> ConsolidationContainers;

//    @JsonProperty("CommonContainers
//    private List<CommonContainersRequest> CommonContainers;

    // @JsonProperty("Shipments
    // private List<ShipmentsRow> Shipments;
//    @JsonProperty("Routings
//    private List<RoutingsRow> Routings;
    @JsonProperty("SendingAgentId")
    private Long SendingAgentId;
    @JsonProperty("SendingAgentAddressId")
    private Integer SendingAgentAddressId;
    @JsonProperty("ReceivingAgentId")
    private Long ReceivingAgentId;
    @JsonProperty("ReceivingAgentAddressId")
    private Integer ReceivingAgentAddressId;
    @JsonProperty("Carrier")
    private String Carrier;
    @JsonProperty("CreditorId")
    private Long CreditorId;
    @JsonProperty("CreditorAddressId")
    private Integer CreditorAddressId;
    @JsonProperty("CarrierBookingRef")
    private String CarrierBookingRef;
    @JsonProperty("AgentReference")
    private String AgentReference;
    @JsonProperty("CoLoadWithId")
    private Long CoLoadWithId;
    @JsonProperty("CoLoadWithAddressId")
    private Integer CoLoadWithAddressId;
    @JsonProperty("CoLoadMBL")
    private String CoLoadMBL;
    @JsonProperty("CoLoadBookingRef")
    private String CoLoadBookingRef;

    @JsonProperty("dCTOId")
    private Long dCTOId;
    @JsonProperty("dCTOAddressId")
    private Integer dCTOAddressId;
    @JsonProperty("dCFSId")
    private Long dCFSId;
    @JsonProperty("dCYDId")
    private Long dCYDId;
    @JsonProperty("dPortTransportId")
    private Long dPortTransportId;
    @JsonProperty("dCFSAddressId")
    private Integer dCFSAddressId;
    @JsonProperty("dCYDAddressId")
    private Long dCYDAddressId;
    @JsonProperty("dPortTransportAddressId")
    private Long dPortTransportAddressId;
    @JsonProperty("dContainerYardId")
    private Integer dContainerYardId; // Currently not used. Kept for backward compatibility.
    @JsonProperty("dTransportPortId")
    private Integer dTransportPortId;// Currently not used. Kept for backward compatibility.
    @JsonProperty("dFirstForeignPortId")
    private Integer dFirstForeignPortId;
    @JsonProperty("dFirstForeignPortArrivalDate")
    private LocalDateTime dFirstForeignPortArrivalDate;
    @JsonProperty("dLastForeignPortDepartureDate")
    private LocalDateTime dLastForeignPortDepartureDate;
    @JsonProperty("dLastForeignPortId")
    private Integer dLastForeignPortId;

    @JsonProperty("aCTOId")
    private Long aCTOId;
    @JsonProperty("aCTOAddressId")
    private Integer aCTOAddressId;
    @JsonProperty("aCFSId")
    private Long aCFSId;
    @JsonProperty("aCYDId")
    private Long aCYDId;
    @JsonProperty("aPortTransportId")
    private Long aPortTransportId;
    @JsonProperty("aCFSAddressId")
    private Integer aCFSAddressId;
    @JsonProperty("aCYDAddressId")
    private Long aCYDAddressId;
    @JsonProperty("aPortTransportAddressId")
    private Long aPortTransportAddressId;
    @JsonProperty("aContainerYardId")
    private Integer aContainerYardId; // Currently not used. Kept for backward compatibility.
    @JsonProperty("aTransportPortId")
    private Integer aTransportPortId;// Currently not used. Kept for backward compatibility.
    @JsonProperty("aFirstArrivalPortId")
    private Integer aFirstArrivalPortId;
    @JsonProperty("aFirstArrivalPortArrivalDate")
    private LocalDateTime aFirstArrivalPortArrivalDate;
    @JsonProperty("aLastForeignPortDepartureDate")
    private LocalDateTime aLastForeignPortDepartureDate;
    @JsonProperty("aLastForeignPortId")
    private Integer aLastForeignPortId;
    @JsonProperty("ManifestPrint")
    private String ManifestPrint;
    @JsonProperty("PrinOtherDocs")
    private String PrinOtherDocs;
    @JsonProperty("AWBDims")
    private String AWBDims;
    @JsonProperty("ReleaseType")
    private String ReleaseType;
    @JsonProperty("Originals")
    private Integer Originals;
    @JsonProperty("Copy")
    private Integer Copy;
    @JsonProperty("PlaceOfIssue")
    private String PlaceOfIssue;
    @JsonProperty("MasterBillIssueDate")
    private LocalDateTime MasterBillIssueDate;
    @JsonProperty("ShipmentsCount")
    private Long ShipmentsCount;
    @JsonProperty("Weight")
    private BigDecimal Weight;
    @JsonProperty("WeightUnit")
    private String WeightUnit;
    @JsonProperty("Volume")
    private BigDecimal Volume;
    @JsonProperty("VolumeUnit")
    private String VolumeUnit;
    @JsonProperty("Chargeable")
    private BigDecimal Chargeable;
    @JsonProperty("ChargeableUnit")
    private String ChargeableUnit;
    @JsonProperty("CutoffDate")
    private LocalDateTime CutoffDate;
    @JsonProperty("Hazardous")
    private Boolean hazardous;
    @JsonProperty("DGClass")
    private String DGClass;
    @JsonProperty("DGSubstance")
    private String DGSubstance;
    @JsonProperty("IsTemparatureControlled")
    private Boolean IsTemparatureControlled;
    @JsonProperty("MinTemp")
    private BigDecimal MinTemp;
    @JsonProperty("MaxTemp")
    private BigDecimal MaxTemp;
    @JsonProperty("MinTempUnit")
    private String MinTempUnit;
    @JsonProperty("MaxTempUnit")
    private String MaxTempUnit;
    @JsonProperty("Override")
    private boolean Override;
    @JsonProperty("WeightVolume")
    private BigDecimal WeightVolume;
    @JsonProperty("WeightVolumeUnit")
    private String WeightVolumeUnit;
    @JsonProperty("ConsolidatedWeight")
    private BigDecimal ConsolidatedWeight;
    @JsonProperty("ConsolidatedWeightUnit")
    private String ConsolidatedWeightUnit;
    @JsonProperty("WeightUtilization")
    private String WeightUtilization;
    @JsonProperty("ConsolidatiedVolume")
    private BigDecimal ConsolidatiedVolume;
    @JsonProperty("ConsolidatiedVolumeUnit")
    private String ConsolidatiedVolumeUnit;
    @JsonProperty("VolumeUtilization")
    private String VolumeUtilization;
    @JsonProperty("ConsolidationChargeQuantity")
    private BigDecimal ConsolidationChargeQuantity;
    @JsonProperty("ConsolidationChargeQuantityUnit")
    private String ConsolidationChargeQuantityUnit;
//    @JsonProperty("DGClasses
//    private List<DGRow> DGClasses;
//    @JsonProperty("DGSubstances
//    private List<DGSubstanceRow> DGSubstances;
    // @JsonProperty("TempClasses
    // private List<TemperatureClassRow> TempClasses;
//    @JsonProperty("ReferenceNumbers
//    private List<ReferenceNumbersRow> ReferenceNumbers;
//    @JsonProperty("ConsolidationAddresses
//    private List<ConsolidationAddressRow> ConsolidationAddresses;

    @JsonProperty("ShipmentIds")
    private List<Long> ShipmentIds;
    @JsonProperty("TotalContainers")
    private String TotalContainers;
    @JsonProperty("TotalPackages")
    private String TotalPackages;
    @JsonProperty("TotalWeight")
    private String TotalWeight;
    @JsonProperty("EstimatedTerminalCutoff")
    private LocalDateTime EstimatedTerminalCutoff;
    @JsonProperty("TerminalCutoff")
    private LocalDateTime TerminalCutoff;
    @JsonProperty("BookingCutoff")
    private LocalDateTime BookingCutoff;
    @JsonProperty("ShipInstructionCutoff")
    private LocalDateTime ShipInstructionCutoff;
    @JsonProperty("HazardousBookingCutoff")
    private LocalDateTime HazardousBookingCutoff;
    @JsonProperty("VerifiedGrossMassCutoff")
    private LocalDateTime VerifiedGrossMassCutoff;
    @JsonProperty("ReeferCutoff")
    private LocalDateTime ReeferCutoff;
    @JsonProperty("LatestFullEquDeliveredToCarrier")
    private LocalDateTime LatestFullEquDeliveredToCarrier;
    @JsonProperty("EarliestDropOffFullEquToCarrier")
    private LocalDateTime EarliestDropOffFullEquToCarrier;
    @JsonProperty("EarliestEmptyEquPickUp")
    private LocalDateTime EarliestEmptyEquPickUp;

    @JsonProperty("ShipmentType")
    private String ShipmentType;
    @JsonProperty("ReferenceNo")
    private String ReferenceNo;
    @JsonProperty("DeclarationType")
    private String DeclarationType;

    @JsonProperty("Bookingstatus")
    private Integer Bookingstatus;
    @JsonProperty("BookingId")
    private String BookingId;
//    @JsonProperty("ShipmentOrders
//    private List<OrdersRow> ShipmentOrders;
//    @JsonProperty("Docs
//    private List<FileRepoRow> Docs;
    @JsonProperty("IsCharter")
    private boolean IsCharter;
    @JsonProperty("IsCargoOnly")
    private boolean IsCargoOnly;
    @JsonProperty("IsLinked")
    private boolean IsLinked;
//    @JsonProperty("consolidatedBill
//    private List<BillRow> consolidatedBill;

    //for print
    @JsonProperty("SendingAgentName")
    private String SendingAgentName;
    @JsonProperty("SendingAgentAddress1")
    private String SendingAgentAddress1;
    @JsonProperty("SendingAgentAddress2")
    private String SendingAgentAddress2;
    @JsonProperty("SendingAgentCity")
    private String SendingAgentCity;
    @JsonProperty("SendingAgentCountry")
    private String SendingAgentCountry;
    @JsonProperty("SendingAgentEmail")
    private String SendingAgentEmail;
    @JsonProperty("SendingAgentContactPhone")
    private String SendingAgentContactPhone;
    @JsonProperty("SendingAgentFaxNumber")
    private String SendingAgentFaxNumber;

    @JsonProperty("ReceivingAgentName")
    private String ReceivingAgentName;
    @JsonProperty("ReceivinAgentAddress1")
    private String ReceivinAgentAddress1;
    @JsonProperty("ReceivingAgentContactPhone")
    private String ReceivingAgentContactPhone;
    @JsonProperty("ReceivingAgentEmail")
    private String ReceivingAgentEmail;
    @JsonProperty("ReceivingAgentCountry")
    private String ReceivingAgentCountry;
    @JsonProperty("ReceivingAgentCity")
    private String ReceivingAgentCity;
    @JsonProperty("ReceivinAgentAddress2")
    private String ReceivinAgentAddress2;
    @JsonProperty("ReceivingAgentFaxNumber")
    private String ReceivingAgentFaxNumber;

    @JsonProperty("CTOName")
    private String CTOName;
    @JsonProperty("CTOAddress1")
    private String CTOAddress1;
    @JsonProperty("CTOContactPhone")
    private String CTOContactPhone;
    @JsonProperty("CTOEmail")
    private String CTOEmail;
    @JsonProperty("CTOCountry")
    private String CTOCountry;
    @JsonProperty("CTOCity")
    private String CTOCity;
    @JsonProperty("CTOAddress2")
    private String CTOAddress2;
    @JsonProperty("CTOFaxPhone")
    private String CTOFaxPhone;
    @JsonProperty("IsLocked")
    private boolean IsLocked;

    //Consolidation Address
    @JsonProperty("ConsolidationAddress")
    private List<PartyRequestV2> consolidationAddresses;
    @JsonProperty("ModeOfBooking")
    private String ModeOfBooking;
    @JsonProperty("AutoUpdateGoodsDesc")
    private Boolean autoUpdateGoodsDesc;
    @JsonProperty("CreatedBy")
    private String CreatedBy;
    @JsonProperty("CreatedDate")
    private LocalDateTime CreatedDate;
    @JsonProperty("EmergencyContactNumber")
    private String emergencyContactNumber;
    @JsonProperty("EmergencyContactNumberCode")
    private String emergencyContactNumberCode;
}
