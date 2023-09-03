package com.dpw.runner.shipment.services.syncing.Entity;

import lombok.Data;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Data
public class CustomShipmentRequest {

    public UUID Guid;
    public String HouseBill;
    public String TransportMode;
    public String Custom_ShipType;
    public String ContainerType;
    public String Source;
    public String MasterBill;
    public Boolean IsDomestic;
    public BigDecimal Weight;
    public String WeightUnit;
    public Integer Inners;
    public String InnersUnit;
    public BigDecimal FreightLocal;
    public String FreightLocalCurrency;
    public BigDecimal FreightOverseas;
    public String FreightOverseasCurrency;
    public String EntryDetail;
    public String AdditionalTerms;
    public String Description;

    public BigDecimal Volume;
    public String VolumeUnit;
    public BigDecimal VolumeWeight;
    public String WeightVolumeUnit;
    public BigDecimal Chargeable;
    public String ChargableUnit;
    public Integer Packs;
    public String PacksUnit;
    public String ShipmentId;

    public LocalDateTime Eta;
    public LocalDateTime Etd;
    public String OriginName;
    public String Voyage;
    public String AirwayBillDims;
    public String EfreightStatus;

    public String HouseBillType;
    public String Inspection;
    public String OnBoard;
    public String Phase;

    public Long PlaceofIssue;
    public Long PlaceofSupply;
    public String ReleaseType;
    public String ScreeningStatus;
    public BigDecimal ShipperCod;
    public String ShipperCodpm;

    public BigDecimal SpotRate;
    public String SpotRateType;
    public LocalDateTime CustomsNoIssueDate;
    public LocalDateTime DateofIssue;
    public LocalDateTime ExpiryDate;
    public LocalDateTime OnBoardDate;



    // %%%%%%%%%%%%%% Custom shipment properties from here %%%%%%%%%%%%%%%%
    //Jobs
    public List<JobRequestV2> JobsList;

    public List<TruckDriverDetailsRequestV2> TruckDriverDetail; // present

    //Carrier details here
    public String PaidPlaceName;
    public String AircraftRegistration;
    public String AircraftType;
    public LocalDateTime ATA;
    public LocalDateTime ATD;
    public String Destination;
    public String FlightNumber;
    public String JourneyNumber;
    public String JourneyRefNumber;
    public String ShippingLine;
    public String TruckRefNumber;
    public String Vessel;

    //adding addtional details here
    public String ActivityType;
    public String Adcode;
    public LocalDateTime AndesResponseDate;
    public String AndesStatusString;
    public String AndesStatusResponseText;
    public String AndesTicket;
    public BigDecimal AssessValue;
    public String Betype;
    public String BlchargesDisplay;//TODO : REMOVE
    public String BlexporterShipment;
    public LocalDateTime Boedate;
    public String Boenumber;
    public Long BondedWarehouseId; //int64
    public PartyRequestV2 BorrowedFrom;
    public String BranchSINumber;
    public String ChajobNumber;
    public Long Cifvalue; //int64
    public Long Copy; //int64
    public String CustomCity;
    public String CustomHouse;
    public String CustomLocation;

    public LocalDateTime DateOfReceipt;
    public String DeliveryMode; // TODO : REMOVE
    public Boolean DraftPrinted;

    public PartyRequestV2 Etailor;

    public PartyRequestV2 ExportBroker;
    public String ExternalNotes;
    public Long FreeDays; //int64
    public String GoodsCO;

    public Long HsnNumber; //int64
    public String Iecode;
    public LocalDateTime IgmfileDate;
    public String IgmfileNo;
    public LocalDateTime IgminwardDate;
    public PartyRequestV2 ImportBroker;
    public Boolean ImportExportShipmentLock;

    public BigDecimal InvoiceValue;
    public LocalDateTime InwardDateAndTime;
    public Boolean IsCmsHBLSent;
    public Boolean IsCreditOverrideApproved;
    public Boolean IsExportClearance;
    public Boolean IsImportClearance;
    public Boolean IsInland;
    public String LgdStatus;
    public Long LineNumber; //int64
    public Long LocalLineNumber; //int64
    public PartyRequestV2 NotifyParty;


    public Long Original; //int64
    public String OwnershipString;
    public String OwnershipName;

    public String PassedByString;
    public String PassedByPerson;
    public String PeruEntryExitPoint;


    public Boolean PrintedOriginal;
    public PartyRequestV2 ReceivingAgent;
    public PartyRequestV2 ReceivingForwarderParty;
    public PartyRequestV2 SendingAgent;
    public PartyRequestV2 SendingForwarderParty;
    public LocalDateTime Smtpigmdate;
    public String Smtpigmnumber;
    public Long SubLineNumber; //int64
    public LocalDateTime SupplierInvoiceDate;
    public String SupplierInvoiceNumber;
    public Boolean SurrenderPrinted;
    public String TipoDocumentConsignee;
    public String TipoDocumentNotifyParty;
    public BigDecimal TotalDuty;
    public PartyRequestV2 TraderOrSupplierParty;
    public Long WarehouseId; //int64
    public Boolean Wblprinted;
    // ---- Additional Details ends here
    public String StatusString;
    public String PrevShipmentStatusString;


    public List<ContainerRequestV2> ContainersList;
    public PickupDeliveryDetailsRequestV2 PickupDetails;
    public PickupDeliveryDetailsRequestV2 DeliveryDetails;
    public List<NoteRequestV2> NotesList;


    // Missing Properties
    public PartyRequestV2 ConsignerParty;
    public PartyRequestV2 ConsigneeParty;
    public PartyRequestV2 Client;
    public String AssignedTo;
    public Boolean AutoUpdateWtVol;

    public String BookingNumber;
    //bookingReference
    // public String ReferenceNo;
    public String BookingType;
    public Boolean CargoFinanceBooking;
    // public BigDecimal Chargeable;
    // public String ChargableUnit;

    public String ConsolidationReferenceNumber;
    public Boolean ContainerAutoWeightVolumeUpdate;
    //Direction
    // public String Custom_ShipType;
    public Integer DocumentationPartner;
    public String FinanceClosedByUser;
    public LocalDateTime FinanceClosedOn;
    //GoodsDescription
    // public String Description;

    // public String IncoTerm;
    //innerPackUnit
    // public String InnersUnit;
    public Boolean IntraBranch;
    public Boolean IsLocked;
    public Boolean IsNotifyConsigneeEqual;
    public Boolean IsShipmentReadOnly;

    public Integer LockedBy;
     public String MarksnNums;
    public BigDecimal NetWeight;
    public String NetWeightUnit;

    public String PaymentTerms;
    public Integer PrevShipmentStatus;
    public Integer ReceivingBranch;

    // "route": "string",   // ---Missing--- Available in shipmentsRow (RouteId)
    public Long RouteId;
    public Long SalesAgentId;
    public String LockedByUser;
    public String Route;
    public String Incoterms;
    public String JobType;
    public String ServiceType;
    public String ShipmentCompletedByUser;
    public String notifyConsigneeEqual;
    public String ShipmentReadOnly;
    public String OriginPortName;
    public String TenantCode;
    public String DestinationPortName;
    public String TipoDocumentConsignor;
    public String DestinationName;



    // public Integer ShipmentCompletedBy;
    public LocalDateTime ShipmentCompletedOn;
    public Integer SourceTenantId;
    public Integer TriangulationPartner;

    public List<BookingCarriageRequestV2> BookingCarriages;

    // Carrier, Client discuss w/ chirag, Container object clarity rqd
    public List<ElDetailsRequestV2> ELDetails;
    public List<EventsRequestV2> EventsList;
    public List<FileRepoRequestV2> Docs_;
    // Packing object doubt ? discuss w/ chirag
    public List<PackingRequestV2> Packings_;
    public List<ReferenceNumbersRequestV2> ReferenceNumbers;
    public List<RoutingsRequestV2> Routings;
    public List<ShipmentServiceRequestV2> ServicesList;
}