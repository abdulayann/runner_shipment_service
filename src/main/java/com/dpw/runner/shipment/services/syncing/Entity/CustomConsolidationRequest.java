package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Data
public class CustomConsolidationRequest implements IRunnerResponse, IRunnerRequest {

    @JsonProperty
    private AchievedQuantities AchievedQuantities;
    @JsonProperty
    private String AdditionalTerms;
    @JsonProperty
    private Allocations Allocations;
    @JsonProperty
    private ArrivalDepartureDetails ArrivalDepartureDetails;

    // @JsonProperty
    // private Integer Copy;

    // Carrier details
    @JsonProperty
    private String DestinationName;
    @JsonProperty
    private String DestinationPortName;
    @JsonProperty
    private String OriginPortName;
    @JsonProperty
    private String OriginName;
    @JsonProperty
    private String ShipmentId;
    @JsonProperty
    private String ShippingLine;
    @JsonProperty
    private String Voyage;

    @JsonProperty
    private List<ContainerRequestV2> ContainersList;

    @JsonProperty
    private Long BondedWarehouseId;

    @JsonProperty
    private LocalDateTime CargoClosingTime;

    @JsonProperty
    private BigDecimal ConsolidatedVolume;
    @JsonProperty
    private LocalDateTime DocsClosingTime;
    @JsonProperty
    private Integer DocumentationPartner;
    @JsonProperty
    private LocalDateTime DOIssueDate;
    @JsonProperty
    private String DOPlaceOfIssueName;
    @JsonProperty
    private String EDITransactionId;
    // @JsonProperty
    // private LocalDateTime EstimatedTerminalCutoff;

    @JsonProperty
    private UUID Guid;
    @JsonProperty
    private Long Id;
    @JsonProperty
    private Boolean IntraBranch;
    @JsonProperty
    private LocalDateTime InwardDateandTime;

    @JsonProperty
    private Boolean IsInland;
    @JsonProperty
    private Boolean IsReceivingAgentFreeTextAddress;
    @JsonProperty
    private Boolean IsSendingAgentFreeTextAddress;

    @JsonProperty
    private Integer LockedBy;
    @JsonProperty
    private String MarksnNums;
    @JsonProperty
    private String MrnNumber;
    @JsonProperty
    private Long MsnNumber;

    @JsonProperty
    private String PackageType;

    @JsonProperty
    private String ReceivingAgentFreeTextAddress;
    @JsonProperty
    private Integer ReceivingBranch;
    @JsonProperty
    private String ReferenceNumber;

    @JsonProperty
    private String SendingAgentFreeTextAddress;
    @JsonProperty
    private LocalDateTime Smtpigmdate;
    @JsonProperty
    private String Smtpigmnumber;
    @JsonProperty
    private Integer SourceTenantId;
    @JsonProperty
    private String SpecialInstructions;
    @JsonProperty
    private Integer TriangulationPartner;
    @JsonProperty
    private Long WarehouseId;

    @JsonProperty
    private List<JobRequestV2> JobsList;
    @JsonProperty
    private List<NoteRequestV2> NotesList;
    @JsonProperty
    private List<BookingCarriageRequestV2> BookingCarriagesListE;
    @JsonProperty
    private List<ElDetailsRequestV2> ElDetailsList;
    @JsonProperty
    private List<EventsRequestV2> EventsList;
    @JsonProperty
    private List<FileRepoRequestV2> DocsList;
    @JsonProperty
    private List<PackingRequestV2> PackingList;
    @JsonProperty
    private List<ReferenceNumbersRequestV2> ReferenceNumbersList;
    @JsonProperty
    private List<RoutingsRequestV2> RoutingsList;
    @JsonProperty
    private List<CustomShipmentRequest> ShipmentList;
    @JsonProperty
    private List<TruckDriverDetailsRequestV2> TruckDriverDetail;

    @JsonProperty
    private String FirstLoadString;
    @JsonProperty
    private String LastDischargeString;
    @JsonProperty
    private String IGMFileNo;
    @JsonProperty
    private LocalDateTime IGMFileDate;
    @JsonProperty
    private LocalDateTime IGMInwardDate;
    @JsonProperty
    private LocalDateTime SMTPIGMDate;
    @JsonProperty
    private String SMTPIGMNumber;

    @JsonProperty
    private List<UUID> ShipmentGuids;

    @JsonProperty
    private PartyRequestV2 SendingAgent;
    @JsonProperty
    private PartyRequestV2 ReceivingAgent;
    @JsonProperty
    private PartyRequestV2 BorrowedFrom;
    @JsonProperty
    private PartyRequestV2 Creditor;
    @JsonProperty
    private PartyRequestV2 CoLoadWith;

    @JsonProperty
    private String Description;
    @JsonProperty
    private String PlaceOfIssueString;

    ///////// Original Consol Request


    @JsonProperty
    private List<String> shipmentRefNumbers;
    @JsonProperty
    private String Type;
    @JsonProperty
    private String ConsolidationNumber;
    // @JsonProperty
    // private Long ConsolidationId;
    @JsonProperty
    private String TransportMode;
    @JsonProperty
    private String DeliveryMode;
    @JsonProperty
    private String ContainerType;
    @JsonProperty
    private String FirstLoad;
    @JsonProperty
    private String LastDischarge;
    @JsonProperty
    private String POLId;
    @JsonProperty
    private String PODId;
    @JsonProperty
    private LocalDateTime Eta;
    @JsonProperty
    private LocalDateTime Ata;
    @JsonProperty
    private LocalDateTime Etd;
    @JsonProperty
    private LocalDateTime Atd;
    @JsonProperty
    private Boolean IsDomestic;
    @JsonProperty
    private String VoyageNumber;
    @JsonProperty
    private String Vessel;
    @JsonProperty
    private String FlightNumber;
    @JsonProperty
    private String AircraftType;
    @JsonProperty
    private String AircraftRegistration;
    @JsonProperty
    private String TruckRefNumber;
    @JsonProperty
    private String OtherInfo;
    @JsonProperty
    private String JourneyNumber;
    @JsonProperty
    private String JourneyRefNumber;
    @JsonProperty
    private String Bol;
    @JsonProperty
    private String MAWB;
    @JsonProperty
    private String ServiceLevel;
    @JsonProperty
    private String Payment;
//    @JsonProperty
//    private List<ConsolidationContainersRow> ConsolidationContainers;

//    @JsonProperty
//    private List<CommonContainersRequest> CommonContainers;

    // @JsonProperty
    // private List<ShipmentsRow> Shipments;
//    @JsonProperty
//    private List<RoutingsRow> Routings;
    @JsonProperty
    private Long SendingAgentId;
    @JsonProperty
    private Integer SendingAgentAddressId;
    @JsonProperty
    private Long ReceivingAgentId;
    @JsonProperty
    private Integer ReceivingAgentAddressId;
    @JsonProperty
    private String Carrier;
    @JsonProperty
    private Long CreditorId;
    @JsonProperty
    private Integer CreditorAddressId;
    @JsonProperty
    private String CarrierBookingRef;
    @JsonProperty
    private String AgentReference;
    @JsonProperty
    private Long CoLoadWithId;
    @JsonProperty
    private Integer CoLoadWithAddressId;
    @JsonProperty
    private String CoLoadMBL;
    @JsonProperty
    private String CoLoadBookingRef;

    @JsonProperty
    private Long dCTOId;
    @JsonProperty
    private Integer dCTOAddressId;
    @JsonProperty
    private Long dCFSId;
    @JsonProperty
    private Long dCYDId;
    @JsonProperty
    private Long dPortTransportId;
    @JsonProperty
    private Integer dCFSAddressId;
    @JsonProperty
    private Long dCYDAddressId;
    @JsonProperty
    private Long dPortTransportAddressId;
    @JsonProperty
    private Integer dContainerYardId; // Currently not used. Kept for backward compatibility.
    @JsonProperty
    private Integer dTransportPortId;// Currently not used. Kept for backward compatibility.
    @JsonProperty
    private Integer dFirstForeignPortId;
    @JsonProperty
    private LocalDateTime dFirstForeignPortArrivalDate;
    @JsonProperty
    private LocalDateTime dLastForeignPortDepartureDate;
    @JsonProperty
    private Integer dLastForeignPortId;

    @JsonProperty
    private Long aCTOId;
    @JsonProperty
    private Integer aCTOAddressId;
    @JsonProperty
    private Long aCFSId;
    @JsonProperty
    private Long aCYDId;
    @JsonProperty
    private Long aPortTransportId;
    @JsonProperty
    private Integer aCFSAddressId;
    @JsonProperty
    private Long aCYDAddressId;
    @JsonProperty
    private Long aPortTransportAddressId;
    @JsonProperty
    private Integer aContainerYardId; // Currently not used. Kept for backward compatibility.
    @JsonProperty
    private Integer aTransportPortId;// Currently not used. Kept for backward compatibility.
    @JsonProperty
    private Integer aFirstArrivalPortId;
    @JsonProperty
    private LocalDateTime aFirstArrivalPortArrivalDate;
    @JsonProperty
    private LocalDateTime aLastForeignPortDepartureDate;
    @JsonProperty
    private Integer aLastForeignPortId;
    @JsonProperty
    private String ManifestPrint;
    @JsonProperty
    private String PrinOtherDocs;
    @JsonProperty
    private String AWBDims;
    @JsonProperty
    private String ReleaseType;
    @JsonProperty
    private Integer Originals;
    @JsonProperty
    private Integer Copy;
    @JsonProperty
    private String PlaceOfIssue;
    @JsonProperty
    private LocalDateTime MasterBillIssueDate;
    @JsonProperty
    private Long ShipmentsCount;
    @JsonProperty
    private BigDecimal Weight;
    @JsonProperty
    private String WeightUnit;
    @JsonProperty
    private BigDecimal Volume;
    @JsonProperty
    private String VolumeUnit;
    @JsonProperty
    private BigDecimal Chargeable;
    @JsonProperty
    private String ChargeableUnit;
    @JsonProperty
    private LocalDateTime CutoffDate;
    @JsonProperty
    private Boolean Hazardous;
    @JsonProperty
    private String DGClass;
    @JsonProperty
    private String DGSubstance;
    @JsonProperty
    private Boolean IsTemparatureControlled;
    @JsonProperty
    private BigDecimal MinTemp;
    @JsonProperty
    private BigDecimal MaxTemp;
    @JsonProperty
    private String MinTempUnit;
    @JsonProperty
    private String MaxTempUnit;
    @JsonProperty
    private Boolean Override;
    @JsonProperty
    private BigDecimal WeightVolume;
    @JsonProperty
    private String WeightVolumeUnit;
    @JsonProperty
    private BigDecimal ConsolidatedWeight;
    @JsonProperty
    private String ConsolidatedWeightUnit;
    @JsonProperty
    private String WeightUtilization;
    @JsonProperty
    private BigDecimal ConsolidatiedVolume;
    @JsonProperty
    private String ConsolidatiedVolumeUnit;
    @JsonProperty
    private String VolumeUtilization;
    @JsonProperty
    private BigDecimal ConsolidationChargeQuantity;
    @JsonProperty
    private String ConsolidationChargeQuantityUnit;
//    @JsonProperty
//    private List<DGRow> DGClasses;
//    @JsonProperty
//    private List<DGSubstanceRow> DGSubstances;
    // @JsonProperty
    // private List<TemperatureClassRow> TempClasses;
//    @JsonProperty
//    private List<ReferenceNumbersRow> ReferenceNumbers;
//    @JsonProperty
//    private List<ConsolidationAddressRow> ConsolidationAddresses;

    @JsonProperty
    private List<Long> ShipmentIds;
    @JsonProperty
    private String TotalContainers;
    @JsonProperty
    private String TotalPackages;
    @JsonProperty
    private String TotalWeight;
    @JsonProperty
    private LocalDateTime EstimatedTerminalCutoff;
    @JsonProperty
    private LocalDateTime TerminalCutoff;
    @JsonProperty
    private LocalDateTime BookingCutoff;
    @JsonProperty
    private LocalDateTime ShipInstructionCutoff;
    @JsonProperty
    private LocalDateTime HazardousBookingCutoff;
    @JsonProperty
    private LocalDateTime VerifiedGrossMassCutoff;
    @JsonProperty
    private LocalDateTime ReeferCutoff;

    @JsonProperty
    private String ShipmentType;
    @JsonProperty
    private String ReferenceNo;
    @JsonProperty
    private String DeclarationType;

    @JsonProperty
    private Integer Bookingstatus;
    @JsonProperty
    private String BookingId;
//    @JsonProperty
//    private List<OrdersRow> ShipmentOrders;
//    @JsonProperty
//    private List<FileRepoRow> Docs;
    @JsonProperty
    private Boolean IsCharter;
    @JsonProperty
    private Boolean IsCargoOnly;
    @JsonProperty
    private Boolean IsLinked;
//    @JsonProperty
//    private List<BillRow> consolidatedBill;

    //for print
    @JsonProperty
    private String SendingAgentName;
    @JsonProperty
    private String SendingAgentAddress1;
    @JsonProperty
    private String SendingAgentAddress2;
    @JsonProperty
    private String SendingAgentCity;
    @JsonProperty
    private String SendingAgentCountry;
    @JsonProperty
    private String SendingAgentEmail;
    @JsonProperty
    private String SendingAgentContactPhone;
    @JsonProperty
    private String SendingAgentFaxNumber;

    @JsonProperty
    private String ReceivingAgentName;
    @JsonProperty
    private String ReceivinAgentAddress1;
    @JsonProperty
    private String ReceivingAgentContactPhone;
    @JsonProperty
    private String ReceivingAgentEmail;
    @JsonProperty
    private String ReceivingAgentCountry;
    @JsonProperty
    private String ReceivingAgentCity;
    @JsonProperty
    private String ReceivinAgentAddress2;
    @JsonProperty
    private String ReceivingAgentFaxNumber;

    @JsonProperty
    private String CTOName;
    @JsonProperty
    private String CTOAddress1;
    @JsonProperty
    private String CTOContactPhone;
    @JsonProperty
    private String CTOEmail;
    @JsonProperty
    private String CTOCountry;
    @JsonProperty
    private String CTOCity;
    @JsonProperty
    private String CTOAddress2;
    @JsonProperty
    private String CTOFaxPhone;
    @JsonProperty
    private Boolean IsLocked;
}
