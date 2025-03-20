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
public class CustomShipmentSyncRequest implements IRunnerRequest, IRunnerResponse {

    @JsonProperty("Guid")
    private UUID Guid;
    @JsonProperty("SourceGuid")
    private UUID SourceGuid;
    @JsonProperty("ConsolidationGuids")
    private Map<UUID, Integer> consolidationGuids;
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
    private boolean IsDomestic;
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
    @JsonProperty("Commodity")
    private String Commodity;

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
    @JsonProperty("IssueDate")
    private LocalDateTime IssueDate;
    @JsonProperty("ExpiryDate")
    private LocalDateTime ExpiryDate;
    @JsonProperty("OnBoardDate")
    private LocalDateTime OnBoardDate;

    @JsonProperty("GoodsValue")
    private BigDecimal goodsValue;
    @JsonProperty("GoodsValueCurrency")
    private String goodsValueCurrency;
    @JsonProperty("InsuranceValue")
    private BigDecimal insuranceValue;
    @JsonProperty("InsuranceValueCurrency")
    private String InsuranceValueCurrency;
    @JsonProperty("IsConsignerFreeTextAddress")
    private Boolean IsConsignerFreeTextAddress;
    @JsonProperty("ConsignerFreeTextAddress")
    private String ConsignerFreeTextAddress;
    @JsonProperty("IsConsigneeFreeTextAddress")
    private Boolean IsConsigneeFreeTextAddress;
    @JsonProperty("ConsigneeFreeTextAddress")
    private String ConsigneeFreeTextAddress;
    @JsonProperty("IsNotifyPartyFreeTextAddress")
    private Boolean IsNotifyPartyFreeTextAddress;
    @JsonProperty("NotifyPartyFreeTextAddress")
    private String NotifyPartyFreeTextAddress;

    // %%%%%%%%%%%%%% Custom shipment properties from here %%%%%%%%%%%%%%%%

    //Carrier details here
    @JsonProperty("PaidPlaceName")
    private String PaidPlaceName;
    @JsonProperty("PlaceOfIssueName")
    private String PlaceOfIssueName;
    @JsonProperty("PlaceOfSupplyName")
    private String PlaceOfSupplyName;
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
    @JsonProperty("FlightStatus")
    private String flightStatus;
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
    @JsonProperty("ChargesApply")
    private String ChargesApply;
    @JsonProperty("ExporterStmt")
    private String ExporterStmt;
    @JsonProperty("Boedate")
    private LocalDateTime Boedate;
    @JsonProperty("Boenumber")
    private String Boenumber;
    @JsonProperty("BondedWarehouseId")
    private Long BondedWarehouseId;
    @JsonProperty("BorrowedFrom")
    private PartyRequestV2 BorrowedFrom;
    @JsonProperty("BranchSINumber")
    private String BranchSINumber;
    @JsonProperty("ChajobNumber")
    private String ChajobNumber;
    @JsonProperty("CIFValue")
    private BigDecimal cIFValue;
    @JsonProperty("Copy")
    private Integer copy;
    @JsonProperty("CustomCity")
    private String CustomCity;
    @JsonProperty("CustomHouse")
    private String CustomHouse;
    @JsonProperty("CustomLocation")
    private String CustomLocation;

    @JsonProperty("DateofReceipt")
    private LocalDateTime DateofReceipt;
    @JsonProperty("DraftPrinted")
    private boolean DraftPrinted;

    @JsonProperty("Etailor")
    private PartyRequestV2 Etailor;
    @JsonProperty("ExportBroker")
    private PartyRequestV2 ExportBroker;
    @JsonProperty("ExternalNotes")
    private String ExternalNotes;
    @JsonProperty("FreeDays")
    private BigDecimal freeDays;
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
    private boolean ImportExportShipmentLock;
    @JsonProperty("InvoiceValue")
    private BigDecimal InvoiceValue;
    @JsonProperty("InwardDateAndTime")
    private LocalDateTime InwardDateAndTime;
    @JsonProperty("IsCmsHBLSent")
    private boolean IsCmsHBLSent;
    @JsonProperty("IsCreditOverrideApproved")
    private boolean IsCreditOverrideApproved;
    @JsonProperty("IsExportClearance")
    private boolean IsExportClearance;
    @JsonProperty("IsImportClearance")
    private boolean IsImportClearance;
    @JsonProperty("IsInland")
    private boolean IsInland;
    @JsonProperty("LgdStatus")
    private String LgdStatus;
    @JsonProperty("LineNumber")
    private Long LineNumber; //int64
    @JsonProperty("LocalLineNumber")
    private Long LocalLineNumber; //int64
    @JsonProperty("NotifyParty")
    private PartyRequestV2 NotifyParty;
    @JsonProperty("Original")
    private Integer original;
    @JsonProperty("OwnershipString")
    private String OwnershipString;
    @JsonProperty("OwnershipName")
    private String OwnershipName;
    @JsonProperty("OwnershipParty")
    private PartyRequestV2 OwnershipParty;
    @JsonProperty("PassedByString")
    private String PassedByString;
    @JsonProperty("PassedByPerson")
    private String PassedByPerson;
    @JsonProperty("PeruEntryExitPoint")
    private String PeruEntryExitPoint;
    @JsonProperty("PrintedOriginal")
    private boolean PrintedOriginal;
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
    private boolean SurrenderPrinted;
    @JsonProperty("TipoDocumentConsignee")
    private String TipoDocumentConsignee;
    @JsonProperty("TipoDocumentNotifyParty")
    private String TipoDocumentNotifyParty;
    @JsonProperty("TotalDuty")
    private BigDecimal TotalDuty;
    @JsonProperty("TraderOrSupplierParty")
    private PartyRequestV2 TraderOrSupplierParty;
    @JsonProperty("WarehouseId")
    private Long WarehouseId;
    @JsonProperty("Wblprinted")
    private boolean Wblprinted;
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
    private boolean AutoUpdateWtVol;

    @JsonProperty("BookingNumber")
    private String BookingNumber;
    //bookingReference
    @JsonProperty("ReferenceNo")
    private String ReferenceNo;
    @JsonProperty("BookingType")
    private String BookingType;
    @JsonProperty("CargoFinanceBooking")
    private boolean CargoFinanceBooking;

    @JsonProperty("ConsolidationReferenceNumber")
    private String ConsolidationReferenceNumber;
    @JsonProperty("ContainerAutoWeightVolumeUpdate")
    private boolean ContainerAutoWeightVolumeUpdate;
    @JsonProperty("DocumentationPartner")
    private Integer DocumentationPartner;
    @JsonProperty("FinanceClosedByUser")
    private String FinanceClosedByUser;
    @JsonProperty("FinanceClosedOn")
    private LocalDateTime FinanceClosedOn;
    @JsonProperty("IntraBranch")
    private boolean IntraBranch;
    @JsonProperty("IsLocked")
    private boolean IsLocked;
    @JsonProperty("IsNotifyConsigneeEqual")
    private boolean IsNotifyConsigneeEqual;
    @JsonProperty("IsShipmentReadOnly")
    private boolean IsShipmentReadOnly;

    @JsonProperty("MarksnNums")
    private String MarksnNums;
    @JsonProperty("NetWeight")
    private BigDecimal NetWeight;
    @JsonProperty("NetWeightUnit")
    private String NetWeightUnit;

    @JsonProperty("PaymentTerms")
    private String PaymentTerms;
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

    @JsonProperty("ShipmentCompletedOn")
    private LocalDateTime ShipmentCompletedOn;
    @JsonProperty("SourceTenantId")
    private Integer SourceTenantId;

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
    @JsonProperty("CustomerBookingNotesList")
    private List<NoteRequestV2> CustomerBookingNotesList;
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

    //Shipment Addresses
    @JsonProperty("ShipmentAddress")
    private List<PartyRequestV2> shipmentAddresses;

    @JsonProperty("HblDeliveryMode")
    private String HblDeliveryMode;
    @JsonProperty("CreatedBy")
    private String CreatedBy;
    @JsonProperty("VesselBerthingDate")
    private LocalDateTime VesselBerthingDate;
    @JsonProperty("FmcTlcId")
    private String FmcTlcId;
    @JsonProperty("DeletedContGuids")
    private List<UUID> DeletedContGuids;
    @JsonProperty("BLTermsandConditionsId")
    private String BLTermsandConditionsId;
    @JsonProperty("BlComments")
    private String BlComments;
    @JsonProperty("CargoTerms")
    private String CargoTerms;
    @JsonProperty("CargoTermsDescription")
    private String CargoTermsDescription;
    @JsonProperty("BLRemarks")
    private String BLRemarks;
    @JsonProperty("BLRemarksDescription")
    private String BLRemarksDescription;
    @JsonProperty("Summary")
    private String Summary;
    @JsonProperty("IsSummaryUpdated")
    private Boolean IsSummaryUpdated;
    @JsonProperty("JobStatus")
    private String JobStatus;
    @JsonProperty("ConsigneeDpsAddressId")
    private Long ConsigneeDpsAddressId;
    @JsonProperty("ClientDpsAddressId")
    private Long ClientDpsAddressId;
    @JsonProperty("ConsignorDpsAddressId")
    private Long ConsignorDpsAddressId;
    @JsonProperty("NotifyPartyDpsAddressId")
    private Long NotifyPartyDpsAddressId;
    @JsonProperty("ClientCountryFilter")
    private String ClientCountryFilter;
    @JsonProperty("ConsignorCountryFilter")
    private String ConsignorCountryFilter;
    @JsonProperty("ConsigneeCountryFilter")
    private String ConsigneeCountryFilter;
    @JsonProperty("NotifyPartyCountryFilter")
    private String NotifyPartyCountryFilter;
    @JsonProperty("ContractId")
    private String ContractId;
    @JsonProperty("ContractType")
    private String ContractType;
    @JsonProperty("EntryRefNo")
    private String EntryRefNo;
    @JsonProperty("CreatedDate")
    private LocalDateTime CreatedDate;
    @JsonProperty("CustomerCategoryString")
    private String CustomerCategoryString;
    @JsonProperty("PrimarySalesAgentEmail")
    private String primarySalesAgentEmail;
    @JsonProperty("SecondarySalesAgentEmail")
    private String secondarySalesAgentEmail;
    @JsonProperty("SalesBranch")
    private String salesBranch;
    @JsonProperty("CurrentPartyForQuote")
    private String currentPartyForQuote;
    @JsonProperty("Custom_DeclType")
    private String custom_DeclType;
    @JsonProperty("ContainsHazardous")
    private Boolean ContainsHazardous;
    @JsonProperty("ChangeLogs")
    private List<AuditLogRequestV2> ChangeLogs;
    @JsonProperty("EmergencyContactNumber")
    private String emergencyContactNumber;
    @JsonProperty("EmergencyContactNumberCode")
    private String emergencyContactNumberCode;

    // InsertDate/ UpdateDate
    @JsonProperty("InsertDate")
    private LocalDateTime insertDate;
    @JsonProperty("UpdateDate")
    private LocalDateTime updateDate;
    @JsonProperty("OrderNumber")
    private String orderNumber;
    @JsonProperty("OrderManagementNumber")
    private String orderManagementNumber;
}