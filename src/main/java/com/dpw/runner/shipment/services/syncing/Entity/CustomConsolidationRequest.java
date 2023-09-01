package com.dpw.runner.shipment.services.syncing.Entity;

import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Data
public class CustomConsolidationRequest {

    public AchievedQuantities AchievedQuantities;
    public String AdditionalTerms;
    public Allocations Allocations;
    public ArrivalDepartureDetails ArrivalDepartureDetails;

    // public Integer Copy;

    // Carrier details
    public String DestinationName;
    public String DestinationPortName;
    public String OriginPortName;
    public String OriginName;
    public String ShipmentId;
    public String ShippingLine;
    public String Voyage;

    public List<ContainerRequestV2> ContainersList;

    public Long BondedWarehouseId;

    public LocalDateTime CargoClosingTime;

    public BigDecimal ConsolidatedVolume;
    public LocalDateTime DocsClosingTime;
    public Integer DocumentationPartner;
    public LocalDateTime DOIssueDate;
    public String DOPlaceOfIssueName;
    public String EDITransactionId;
    // public LocalDateTime EstimatedTerminalCutoff;

    public UUID Guid;
    public Long Id;
    public Boolean IntraBranch;
    public LocalDateTime InwardDateandTime;

    public Boolean IsInland;
    public Boolean IsReceivingAgentFreeTextAddress;
    public Boolean IsSendingAgentFreeTextAddress;

    public Integer LockedBy;
    public String MarksnNums;
    public String MrnNumber;
    public Long MsnNumber;

    public String PackageType;

    public String ReceivingAgentFreeTextAddress;
    public Integer ReceivingBranch;
    public String ReferenceNumber;

    public String SendingAgentFreeTextAddress;
    public LocalDateTime Smtpigmdate;
    public String Smtpigmnumber;
    public Integer SourceTenantId;
    public String SpecialInstructions;
    public Integer TriangulationPartner;
    public Long WarehouseId;

    public List<JobRequestV2> JobsList;
    public List<NoteRequestV2> NotesList;
    public List<BookingCarriageRequestV2> BookingCarriagesListE;
    public List<ElDetailsRequestV2> ElDetailsList;
    public List<EventsRequestV2> EventsList;
    public List<FileRepoRequestV2> DocsList;
    public List<PackingRequestV2> PackingList;
    public List<ReferenceNumbersRequestV2> ReferenceNumbersList;
    public List<RoutingsRequestV2> RoutingsList;
    public List<CustomShipmentRequest> ShipmentList;
    public List<TruckDriverDetailsRequestV2> TruckDriverDetail;

    public String FirstLoadString;
    public String LastDischargeString;
    public String IGMFileNo;
    public LocalDateTime IGMFileDate;
    public LocalDateTime IGMInwardDate;
    public LocalDateTime SMTPIGMDate;
    public String SMTPIGMNumber;

    public List<UUID> ShipmentGuids;

    public PartyRequestV2 SendingAgent;
    public PartyRequestV2 ReceivingAgent;
    public PartyRequestV2 BorrowedFrom;
    public PartyRequestV2 Creditor;
    public PartyRequestV2 CoLoadWith;

    public String Description;
    public String PlaceOfIssueString;

    ///////// Original Consol Request


    public List<String> shipmentRefNumbers;
    public String Type;
    public String ConsolidationNumber;
    // public Long ConsolidationId;
    public String TransportMode;
    public String DeliveryMode;
    public String ContainerType;
    public String FirstLoad;
    public String LastDischarge;
    public String POLId;
    public String PODId;
    public LocalDateTime Eta;
    public LocalDateTime Ata;
    public LocalDateTime Etd;
    public LocalDateTime Atd;
    public Boolean IsDomestic;
    public String VoyageNumber;
    public String Vessel;
    public String FlightNumber;
    public String AircraftType;
    public String AircraftRegistration;
    public String TruckRefNumber;
    public String OtherInfo;
    public String JourneyNumber;
    public String JourneyRefNumber;
    public String Bol;
    public String MAWB;
    public String ServiceLevel;
    public String Payment;
//    public List<ConsolidationContainersRow> ConsolidationContainers;

//    public List<CommonContainersRequest> CommonContainers;

    // public List<ShipmentsRow> Shipments;
//    public List<RoutingsRow> Routings;
    public Long SendingAgentId;
    public Integer SendingAgentAddressId;
    public Long ReceivingAgentId;
    public Integer ReceivingAgentAddressId;
    public String Carrier;
    public Long CreditorId;
    public Integer CreditorAddressId;
    public String CarrierBookingRef;
    public String AgentReference;
    public Long CoLoadWithId;
    public Integer CoLoadWithAddressId;
    public String CoLoadMBL;
    public String CoLoadBookingRef;

    public Long dCTOId;
    public Integer dCTOAddressId;
    public Long dCFSId;
    public Long dCYDId;
    public Long dPortTransportId;
    public Integer dCFSAddressId;
    public Long dCYDAddressId;
    public Long dPortTransportAddressId;
    public Integer dContainerYardId; // Currently not used. Kept for backward compatibility.
    public Integer dTransportPortId;// Currently not used. Kept for backward compatibility.
    public Integer dFirstForeignPortId;
    public LocalDateTime dFirstForeignPortArrivalDate;
    public LocalDateTime dLastForeignPortDepartureDate;
    public Integer dLastForeignPortId;

    public Long aCTOId;
    public Integer aCTOAddressId;
    public Long aCFSId;
    public Long aCYDId;
    public Long aPortTransportId;
    public Integer aCFSAddressId;
    public Long aCYDAddressId;
    public Long aPortTransportAddressId;
    public Integer aContainerYardId; // Currently not used. Kept for backward compatibility.
    public Integer aTransportPortId;// Currently not used. Kept for backward compatibility.
    public Integer aFirstArrivalPortId;
    public LocalDateTime aFirstArrivalPortArrivalDate;
    public LocalDateTime aLastForeignPortDepartureDate;
    public Integer aLastForeignPortId;
    public String ManifestPrint;
    public String PrinOtherDocs;
    public String AWBDims;
    public String ReleaseType;
    public Integer Originals;
    public Integer Copy;
    public String PlaceOfIssue;
    public LocalDateTime MasterBillIssueDate;
    public Long ShipmentsCount;
    public BigDecimal Weight;
    public String WeightUnit;
    public BigDecimal Volume;
    public String VolumeUnit;
    public BigDecimal Chargeable;
    public String ChargeableUnit;
    public LocalDateTime CutoffDate;
    public Boolean Hazardous;
    public String DGClass;
    public String DGSubstance;
    public Boolean IsTemparatureControlled;
    public BigDecimal MinTemp;
    public BigDecimal MaxTemp;
    public String MinTempUnit;
    public String MaxTempUnit;
    public Boolean Override;
    public BigDecimal WeightVolume;
    public String WeightVolumeUnit;
    public BigDecimal ConsolidatedWeight;
    public String ConsolidatedWeightUnit;
    public String WeightUtilization;
    public BigDecimal ConsolidatiedVolume;
    public String ConsolidatiedVolumeUnit;
    public String VolumeUtilization;
    public BigDecimal ConsolidationChargeQuantity;
    public String ConsolidationChargeQuantityUnit;
//    public List<DGRow> DGClasses;
//    public List<DGSubstanceRow> DGSubstances;
    // public List<TemperatureClassRow> TempClasses;
//    public List<ReferenceNumbersRow> ReferenceNumbers;
//    public List<ConsolidationAddressRow> ConsolidationAddresses;

    public List<Long> ShipmentIds;
    public String TotalContainers;
    public String TotalPackages;
    public String TotalWeight;
    public LocalDateTime EstimatedTerminalCutoff;
    public LocalDateTime TerminalCutoff;
    public LocalDateTime BookingCutoff;
    public LocalDateTime ShipInstructionCutoff;
    public LocalDateTime HazardousBookingCutoff;
    public LocalDateTime VerifiedGrossMassCutoff;
    public LocalDateTime ReeferCutoff;

    public String ShipmentType;
    public String ReferenceNo;
    public String DeclarationType;

    public Integer Bookingstatus;
    public String BookingId;
//    public List<OrdersRow> ShipmentOrders;
//    public List<FileRepoRow> Docs;
    public Boolean IsCharter;
    public Boolean IsCargoOnly;
    public Boolean IsLinked;
//    public List<BillRow> consolidatedBill;

    //for print
    public String SendingAgentName;
    public String SendingAgentAddress1;
    public String SendingAgentAddress2;
    public String SendingAgentCity;
    public String SendingAgentCountry;
    public String SendingAgentEmail;
    public String SendingAgentContactPhone;
    public String SendingAgentFaxNumber;

    public String ReceivingAgentName;
    public String ReceivinAgentAddress1;
    public String ReceivingAgentContactPhone;
    public String ReceivingAgentEmail;
    public String ReceivingAgentCountry;
    public String ReceivingAgentCity;
    public String ReceivinAgentAddress2;
    public String ReceivingAgentFaxNumber;

    public String CTOName;
    public String CTOAddress1;
    public String CTOContactPhone;
    public String CTOEmail;
    public String CTOCountry;
    public String CTOCity;
    public String CTOAddress2;
    public String CTOFaxPhone;
    public Boolean IsLocked;
}
