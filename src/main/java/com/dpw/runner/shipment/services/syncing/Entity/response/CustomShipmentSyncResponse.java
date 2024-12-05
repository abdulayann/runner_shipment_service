package com.dpw.runner.shipment.services.syncing.Entity.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.syncing.Entity.BookingCarriageRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.ContainerRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.ElDetailsRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.EventsRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.FileRepoRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.JobRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.NoteRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.PackingRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.PartyRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.PickupDeliveryDetailsRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.ReferenceNumbersRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.RoutingsRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.ShipmentServiceRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.TruckDriverDetailsRequestV2;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

public class CustomShipmentSyncResponse implements IRunnerResponse {

    @JsonProperty("Guid")
    private UUID Guid;
    @JsonProperty("HouseBill")
    private String HouseBill;
    @JsonProperty("TransportMode")
    private String TransportMode;
    @JsonProperty("Custom_ShipType")
    private String Custom_ShipType;
    @JsonProperty("ContainerType")
    private String ContainerType;
    @JsonProperty("Source")
    private String Source;
    @JsonProperty("MasterBill")
    private String MasterBill;
    @JsonProperty("IsDomestic")
    private Boolean IsDomestic;
    @JsonProperty("Weight")
    private BigDecimal Weight;
    @JsonProperty("WeightUnit")
    private String WeightUnit;
    @JsonProperty("Inners")
    private Integer Inners;
    @JsonProperty("InnersUnit")
    private String InnersUnit;
    @JsonProperty("FreightLocal")
    private BigDecimal FreightLocal;
    @JsonProperty("FreightLocalCurrency")
    private String FreightLocalCurrency;
    @JsonProperty("FreightOverseas")
    private BigDecimal FreightOverseas;
    @JsonProperty("FreightOverseasCurrency")
    private String FreightOverseasCurrency;
    @JsonProperty("EntryDetail")
    private String EntryDetail;
    @JsonProperty("AdditionalTerms")
    private String AdditionalTerms;
    @JsonProperty("Description")
    private String Description;

    @JsonProperty("Volume")
    private BigDecimal Volume;
    @JsonProperty("VolumeUnit")
    private String VolumeUnit;
    @JsonProperty("VolumeWeight")
    private BigDecimal VolumeWeight;
    @JsonProperty("WeightVolumeUnit")
    private String WeightVolumeUnit;
    @JsonProperty("Chargeable")
    private BigDecimal Chargeable;
    @JsonProperty("ChargableUnit")
    private String ChargableUnit;
    @JsonProperty("Packs")
    private Integer Packs;
    @JsonProperty("PacksUnit")
    private String PacksUnit;
    @JsonProperty("ShipmentId")
    private String ShipmentId;

    @JsonProperty("Eta")
    private LocalDateTime Eta;
    @JsonProperty("Etd")
    private LocalDateTime Etd;
    @JsonProperty("OriginName")
    private String OriginName;
    @JsonProperty("Voyage")
    private String Voyage;
    @JsonProperty("AirwayBillDims")
    private String AirwayBillDims;
    @JsonProperty("EfreightStatus")
    private String EfreightStatus;

    @JsonProperty("HouseBillType")
    private String HouseBillType;
    @JsonProperty("Inspection")
    private String Inspection;
    @JsonProperty("OnBoard")
    private String OnBoard;
    @JsonProperty("Phase")
    private String Phase;

    @JsonProperty("PlaceofIssue")
    private Long PlaceofIssue;
    @JsonProperty("PlaceofSupply")
    private Long PlaceofSupply;
    @JsonProperty("ReleaseType")
    private String ReleaseType;
    @JsonProperty("ScreeningStatus")
    private String ScreeningStatus;
    @JsonProperty("ShipperCod")
    private BigDecimal ShipperCod;
    @JsonProperty("ShipperCodpm")
    private String ShipperCodpm;

    @JsonProperty("SpotRate")
    private BigDecimal SpotRate;
    @JsonProperty("SpotRateType")
    private String SpotRateType;
    @JsonProperty("CustomsNoIssueDate")
    private LocalDateTime CustomsNoIssueDate;
    @JsonProperty("DateofIssue")
    private LocalDateTime DateofIssue;
    @JsonProperty("ExpiryDate")
    private LocalDateTime ExpiryDate;
    @JsonProperty("OnBoardDate")
    private LocalDateTime OnBoardDate;



    // %%%%%%%%%%%%%% Custom shipment properties from here %%%%%%%%%%%%%%%%

    //Carrier details here
    @JsonProperty("PaidPlaceName")
    private String PaidPlaceName;
    @JsonProperty("AircraftRegistration")
    private String AircraftRegistration;
    @JsonProperty("AircraftType")
    private String AircraftType;
    @JsonProperty("Ata")
    private LocalDateTime Ata;
    @JsonProperty("Atd")
    private LocalDateTime Atd;
    @JsonProperty("Destination")
    private String Destination;
    @JsonProperty("FlightNumber")
    private String FlightNumber;
    @JsonProperty("JourneyNumber")
    private String JourneyNumber;
    @JsonProperty("JourneyRefNumber")
    private String JourneyRefNumber;
    @JsonProperty("ShippingLine")
    private String ShippingLine;
    @JsonProperty("TruckRefNumber")
    private String TruckRefNumber;
    @JsonProperty("Vessel")
    private String Vessel;

    //adding addtional details here
    @JsonProperty("ActivityType")
    private String ActivityType;
    @JsonProperty("Adcode")
    private String Adcode;
    @JsonProperty("AndesResponseDate")
    private LocalDateTime AndesResponseDate;
    @JsonProperty("AndesStatusString")
    private String AndesStatusString;
    @JsonProperty("AndesStatusResponseText")
    private String AndesStatusResponseText;
    @JsonProperty("AndesTicket")
    private String AndesTicket;
    @JsonProperty("AssessValue")
    private BigDecimal AssessValue;
    @JsonProperty("Betype")
    private String Betype;
    @JsonProperty("BlchargesDisplay")
    private String BlchargesDisplay;//TODO : REMOVE
    @JsonProperty("BlexporterShipment")
    private String BlexporterShipment;
    @JsonProperty("Boedate")
    private LocalDateTime Boedate;
    @JsonProperty("Boenumber")
    private String Boenumber;
    @JsonProperty("BondedWarehouseId")
    private Long BondedWarehouseId; //int64
    @JsonProperty("BorrowedFrom")
    private PartyRequestV2 BorrowedFrom;
    @JsonProperty("BranchSINumber")
    private String BranchSINumber;
    @JsonProperty("ChajobNumber")
    private String ChajobNumber;
    @JsonProperty("Cifvalue")
    private Long Cifvalue; //int64
    @JsonProperty("Copy")
    private Long Copy; //int64
    @JsonProperty("CustomCity")
    private String CustomCity;
    @JsonProperty("CustomHouse")
    private String CustomHouse;
    @JsonProperty("CustomLocation")
    private String CustomLocation;

    @JsonProperty("DateOfReceipt")
    private LocalDateTime DateOfReceipt;
    @JsonProperty("DeliveryMode")
    private String DeliveryMode; // TODO : REMOVE
    @JsonProperty("DraftPrinted")
    private Boolean DraftPrinted;

    @JsonProperty("Etailor")
    private PartyRequestV2 Etailor;
    @JsonProperty("ExportBroker")
    private PartyRequestV2 ExportBroker;
    @JsonProperty("ExternalNotes")
    private String ExternalNotes;
    @JsonProperty("FreeDays")
    private Long FreeDays; //int64
    @JsonProperty("GoodsCO")
    private String GoodsCO;
    @JsonProperty("HsnNumber")
    private Long HsnNumber; //int64
    @JsonProperty("Iecode")
    private String Iecode;
    @JsonProperty("IgmfileDate")
    private LocalDateTime IgmfileDate;
    @JsonProperty("IgmfileNo")
    private String IgmfileNo;
    @JsonProperty("IgminwardDate")
    private LocalDateTime IgminwardDate;
    @JsonProperty("ImportBroker")
    private PartyRequestV2 ImportBroker;
    @JsonProperty("ImportExportShipmentLock")
    private Boolean ImportExportShipmentLock;
    @JsonProperty("InvoiceValue")
    private BigDecimal InvoiceValue;
    @JsonProperty("InwardDateAndTime")
    private LocalDateTime InwardDateAndTime;
    @JsonProperty("IsCmsHBLSent")
    private Boolean IsCmsHBLSent;
    @JsonProperty("IsCreditOverrideApproved")
    private Boolean IsCreditOverrideApproved;
    @JsonProperty("IsExportClearance")
    private Boolean IsExportClearance;
    @JsonProperty("IsImportClearance")
    private Boolean IsImportClearance;
    @JsonProperty("IsInland")
    private Boolean IsInland;
    @JsonProperty("LgdStatus")
    private String LgdStatus;
    @JsonProperty("LineNumber")
    private Long LineNumber; //int64
    @JsonProperty("LocalLineNumber")
    private Long LocalLineNumber; //int64
    @JsonProperty("NotifyParty")
    private PartyRequestV2 NotifyParty;
    @JsonProperty("Original")
    private Long Original; //int64
    @JsonProperty("OwnershipString")
    private String OwnershipString;
    @JsonProperty("OwnershipName")
    private String OwnershipName;
    @JsonProperty("PassedByString")
    private String PassedByString;
    @JsonProperty("PassedByPerson")
    private String PassedByPerson;
    @JsonProperty("PeruEntryExitPoint")
    private String PeruEntryExitPoint;
    @JsonProperty("PrintedOriginal")
    private Boolean PrintedOriginal;
    @JsonProperty("ReceivingAgent")
    private PartyRequestV2 ReceivingAgent;
    @JsonProperty("ReceivingForwarderParty")
    private PartyRequestV2 ReceivingForwarderParty;
    @JsonProperty("SendingAgent")
    private PartyRequestV2 SendingAgent;
    @JsonProperty("SendingForwarderParty")
    private PartyRequestV2 SendingForwarderParty;
    @JsonProperty("Smtpigmdate")
    private LocalDateTime Smtpigmdate;
    @JsonProperty("Smtpigmnumber")
    private String Smtpigmnumber;
    @JsonProperty("SubLineNumber")
    private Long SubLineNumber; //int64
    @JsonProperty("SupplierInvoiceDate")
    private LocalDateTime SupplierInvoiceDate;
    @JsonProperty("SupplierInvoiceNumber")
    private String SupplierInvoiceNumber;
    @JsonProperty("SurrenderPrinted")
    private Boolean SurrenderPrinted;
    @JsonProperty("TipoDocumentConsignee")
    private String TipoDocumentConsignee;
    @JsonProperty("TipoDocumentNotifyParty")
    private String TipoDocumentNotifyParty;
    @JsonProperty("TotalDuty")
    private BigDecimal TotalDuty;
    @JsonProperty("TraderOrSupplierParty")
    private PartyRequestV2 TraderOrSupplierParty;
    @JsonProperty("WarehouseId")
    private Long WarehouseId; //int64
    @JsonProperty("Wblprinted")
    private Boolean Wblprinted;
    // ---- Additional Details ends here


    @JsonProperty("StatusString")
    private String StatusString;
    @JsonProperty("PrevShipmentStatusString")
    private String PrevShipmentStatusString;


    // Missing Properties
    @JsonProperty("ConsignerParty")
    private PartyRequestV2 ConsignerParty;
    @JsonProperty("ConsigneeParty")
    private PartyRequestV2 ConsigneeParty;
    @JsonProperty("Client")
    private PartyRequestV2 Client;
    @JsonProperty("AssignedTo")
    private String AssignedTo;
    @JsonProperty("AutoUpdateWtVol")
    private Boolean AutoUpdateWtVol;

    @JsonProperty("BookingNumber")
    private String BookingNumber;
    //bookingReference
    // private String ReferenceNo;
    @JsonProperty("BookingType")
    private String BookingType;
    @JsonProperty("CargoFinanceBooking")
    private Boolean CargoFinanceBooking;
    // private BigDecimal Chargeable;
    // private String ChargableUnit;

    @JsonProperty("ConsolidationReferenceNumber")
    private String ConsolidationReferenceNumber;
    @JsonProperty("ContainerAutoWeightVolumeUpdate")
    private Boolean ContainerAutoWeightVolumeUpdate;
    //Direction
    // private String Custom_ShipType;
    @JsonProperty("DocumentationPartner")
    private Integer DocumentationPartner;
    @JsonProperty("FinanceClosedByUser")
    private String FinanceClosedByUser;
    @JsonProperty("FinanceClosedOn")
    private LocalDateTime FinanceClosedOn;
    //GoodsDescription
    // private String Description;

    // private String IncoTerm;
    //innerPackUnit
    // private String InnersUnit;
    @JsonProperty("IntraBranch")
    private Boolean IntraBranch;
    @JsonProperty("IsLocked")
    private Boolean IsLocked;
    @JsonProperty("IsNotifyConsigneeEqual")
    private Boolean IsNotifyConsigneeEqual;
    @JsonProperty("IsShipmentReadOnly")
    private Boolean IsShipmentReadOnly;

    @JsonProperty("LockedBy")
    private Integer LockedBy;
    @JsonProperty("MarksnNums")
    private String MarksnNums;
    @JsonProperty("NetWeight")
    private BigDecimal NetWeight;
    @JsonProperty("NetWeightUnit")
    private String NetWeightUnit;

    @JsonProperty("PaymentTerms")
    private String PaymentTerms;
    @JsonProperty("PrevShipmentStatus")
    private Integer PrevShipmentStatus;
    @JsonProperty("ReceivingBranch")
    private Integer ReceivingBranch;

    // "route": "string",   // ---Missing--- Available in shipmentsRow (RouteId)
    @JsonProperty("RouteId")
    private Long RouteId;
    @JsonProperty("SalesAgentId")
    private Long SalesAgentId;
    @JsonProperty("LockedByUser")
    private String LockedByUser;
    @JsonProperty("Route")
    private String Route;
    @JsonProperty("Incoterms")
    private String Incoterms;
    @JsonProperty("JobType")
    private String JobType;
    @JsonProperty("ServiceType")
    private String ServiceType;
    @JsonProperty("ShipmentCompletedByUser")
    private String ShipmentCompletedByUser;
    @JsonProperty("notifyConsigneeEqual")
    private String notifyConsigneeEqual;
    @JsonProperty("ShipmentReadOnly")
    private String ShipmentReadOnly;
    @JsonProperty("OriginPortName")
    private String OriginPortName;
    @JsonProperty("TenantCode")
    private String TenantCode;
    @JsonProperty("DestinationPortName")
    private String DestinationPortName;
    @JsonProperty("TipoDocumentConsignor")
    private String TipoDocumentConsignor;
    @JsonProperty("DestinationName")
    private String DestinationName;



    // private Integer ShipmentCompletedBy;
    @JsonProperty("ShipmentCompletedOn")
    private LocalDateTime ShipmentCompletedOn;
    @JsonProperty("SourceTenantId")
    private Integer SourceTenantId;
    @JsonProperty("TriangulationPartner")
    private Integer TriangulationPartner;



    //Jobs
    @JsonProperty("JobsList")
    private List<JobRequestV2> JobsList;
    @JsonProperty("TruckDriverDetail")
    private List<TruckDriverDetailsRequestV2> TruckDriverDetail; // present
    @JsonProperty("BookingCarriages")
    private List<BookingCarriageRequestV2> BookingCarriages;
    @JsonProperty("ContainersList")
    private List<ContainerRequestV2> ContainersList;
    @JsonProperty("PickupDetails")
    private PickupDeliveryDetailsRequestV2 PickupDetails;
    @JsonProperty("DeliveryDetails")
    private PickupDeliveryDetailsRequestV2 DeliveryDetails;
    @JsonProperty("NotesList")
    private List<NoteRequestV2> NotesList;
    @JsonProperty("ELDetails")
    private List<ElDetailsRequestV2> ELDetails;
    @JsonProperty("EventsList")
    private List<EventsRequestV2> EventsList;
    @JsonProperty("Docs_")
    private List<FileRepoRequestV2> Docs_;
    @JsonProperty("Packings_")
    private List<PackingRequestV2> Packings_;
    @JsonProperty("ReferenceNumbers")
    private List<ReferenceNumbersRequestV2> ReferenceNumbers;
    @JsonProperty("Routings")
    private List<RoutingsRequestV2> Routings;
    @JsonProperty("ServicesList")
    private List<ShipmentServiceRequestV2> ServicesList;
}
