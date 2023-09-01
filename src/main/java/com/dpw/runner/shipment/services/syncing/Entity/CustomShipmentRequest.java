package com.dpw.runner.shipment.services.syncing.Entity;

import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@Data
public class CustomShipmentRequest {
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
    public long BondedWarehouseId;
    public PartyRequestV2 BorrowedFrom;
    public String BranchSINumber;
    public String ChajobNumber;
    public long Cifvalue;
    public long Copy;
    public String CustomCity;
    public String CustomHouse;
    public String CustomLocation;

    public LocalDateTime DateOfReceipt;
    public String DeliveryMode; // TODO : REMOVE
    public Boolean DraftPrinted;

    public PartyRequestV2 Etailor;

    public PartyRequestV2 ExportBroker;
    public String ExternalNotes;
    public long FreeDays;
    public String GoodsCO;

    public long HsnNumber;
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
    public long LineNumber;
    public long LocalLineNumber;
    public PartyRequestV2 NotifyParty;


    public long Original;
    public String OwnershipString;
    public String OwnershipName;

    public String PassedByString;
    public String PassedByPerson;
    public String PeruEntryExitPoint;



    public Boolean PrintedOriginal;
    public PartyRequestV2 ReceivingAgent;
    public PartyRequestV2 ReceivingForwarder;
    public PartyRequestV2 SendingAgent;
    public PartyRequestV2 SendingForwarder;
    public LocalDateTime Smtpigmdate;
    public String Smtpigmnumber;
    public long SubLineNumber;
    public LocalDateTime SupplierInvoiceDate;
    public String SupplierInvoiceNumber;
    public Boolean SurrenderPrinted;
    public String TipoDocumentConsignee;
    public String TipoDocumentNotifyParty;
    public BigDecimal TotalDuty;
    public PartyRequestV2 TraderOrSupplier;
    public long WarehouseId;
    public Boolean Wblprinted;
    // ---- Additional Details ends here
    public String StatusString;
    public String PrevShipmentStatusString;


    public List<ContainerRequestV2> ContainersList;
    public PickupDeliveryDetailsRequestV2 PickupDetails;
    public PickupDeliveryDetailsRequestV2 DeliveryDetails;
    public List<NoteRequestV2> NotesList;


    // Missing Properties
    public PartyRequestV2 Consigner;
    public PartyRequestV2 Consignee;
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
    // public String MarksnNums;
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
    public List<FileRepoRequestV2> Docs;
    // Packing object doubt ? discuss w/ chirag
    public List<PackingRequestV2> Packings;
    public List<ReferenceNumbersRequestV2> ReferenceNumbers;
    public List<RoutingsRequestV2> Routings;
    public List<ShipmentServiceRequestV2> ServicesList;
}