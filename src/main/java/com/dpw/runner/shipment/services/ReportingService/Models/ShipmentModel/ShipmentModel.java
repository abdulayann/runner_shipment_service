package com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel;

import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.config.CustomVolumeValueSerializer;
import com.dpw.runner.shipment.services.config.CustomWeightValueSerializer;
import com.dpw.runner.shipment.services.config.DecimalPlaceValueSerializer;
import com.dpw.runner.shipment.services.config.LocalDateTimeWithTimeZoneSerializer;
import com.dpw.runner.shipment.services.entity.enums.OceanDGStatus;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class ShipmentModel implements IDocumentModel {
    @JsonProperty("Guid")
    private UUID guid;
    @JsonProperty("AdditionalDetails")
    private AdditionalDetailModel additionalDetails;
    @JsonProperty("AdditionalTerms")
    private String additionalTerms;
    @JsonProperty("AssignedTo")
    private String assignedTo;
    @JsonProperty("AutoUpdateWtVol")
    private Boolean autoUpdateWtVol;
    @JsonProperty("BookingCarriagesList")
    private List<BookingCarriageModel> bookingCarriagesList;
    @JsonProperty("BookingNumber")
    private String bookingNumber;
    @JsonProperty("BookingReference")
    private String bookingReference;
    @JsonProperty("BookingType")
    private String bookingType;
    @JsonProperty("CargoFinanceBooking")
    private Boolean cargoFinanceBooking;
    @JsonProperty("CarrierDetails")
    private CarrierDetailModel carrierDetails;
    @JsonProperty("Chargable")
    @JsonSerialize(using = DecimalPlaceValueSerializer.class)
    private BigDecimal chargable;
    @JsonProperty("ChargeableUnit")
    private String chargeableUnit;
    @JsonProperty("Client")
    private PartiesModel client;
    @JsonProperty("Consignee")
    private PartiesModel consignee;
    @JsonProperty("Consigner")
    private PartiesModel consigner;
    @JsonProperty("ConsolRef")
    private String consolRef;
    @JsonProperty("ContainerAutoWeightVolumeUpdate")
    private Boolean containerAutoWeightVolumeUpdate;
    @JsonProperty("ContainersList")
    private List<ContainerModel> containersList = new ArrayList<>();
    @JsonProperty("Direction")
    private String direction;
    @JsonProperty("DocumentationPartner")
    private Long documentationPartner;
    @JsonProperty("ElDetailsList")
    private List<ELDetailsModel> elDetailsList;
    @JsonProperty("EntryDetail")
    private String entryDetail;
    @JsonProperty("EventsList")
    private List<EventsModel> eventsList;
    @JsonProperty("FileRepoList")
    private List<FileRepoModel> fileRepoList;
    @JsonProperty("FinanceClosedBy")
    private String financeClosedBy;
    @JsonProperty("FinanceClosedOn")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime financeClosedOn;
    @JsonProperty("FreightLocal")
    private BigDecimal freightLocal;
    @JsonProperty("FreightLocalCurrency")
    private String freightLocalCurrency;
    @JsonProperty("FreightOverseas")
    private BigDecimal freightOverseas;
    @JsonProperty("FreightOverseasCurrency")
    private String freightOverseasCurrency;
    @JsonProperty("GoodsDescription")
    private String goodsDescription;
    @JsonProperty("HouseBill")
    private String houseBill;
    @JsonProperty("Id")
    private Long id;
    @JsonProperty("Incoterms")
    private String incoterms;
    @JsonProperty("InnersUnit")
    private String innerPackUnit;
    @JsonProperty("Inners")
    private Integer innerPacks;
    @JsonProperty("IntraBranch")
    private Boolean intraBranch;
    @JsonProperty("IsDomestic")
    private Boolean isDomestic;
    @JsonProperty("IsLocked")
    private Boolean isLocked;
    @JsonProperty("IsNotifyConsigneeEqual")
    private Boolean isNotifyConsigneeEqual;
    @JsonProperty("IsShipmentReadOnly")
    private Boolean isShipmentReadOnly;
    @JsonProperty("JobType")
    private String jobType;
    @JsonProperty("JobsList")
    private List<JobModel> jobsList;
    @JsonProperty("LockedBy")
    private String lockedBy;
    @JsonProperty("MarksNum")
    private String marksNum;
    @JsonProperty("MasterBill")
    private String masterBill;
    @JsonProperty("NetWeight")
    @JsonSerialize(using = CustomWeightValueSerializer.class)
    private BigDecimal netWeight;
    @JsonProperty("NetWeightUnit")
    private String netWeightUnit;
    @JsonProperty("NoOfPacks")
    private Integer noOfPacks;
    @JsonProperty("NotesList")
    private List<NotesModel> notesList;
    @JsonProperty("PackingList")
    private List<PackingModel> packingList;
    @JsonProperty("PacksUnit")
    private String packsUnit;
    @JsonProperty("PaymentTerms")
    private String paymentTerms;
    @JsonProperty("PickupDetails")
    private PickupDeliveryDetailsModel pickupDetails;
    @JsonProperty("DeliveryDetails")
    private PickupDeliveryDetailsModel deliveryDetails;
    @JsonProperty("PrevShipmentStatus")
    private Integer prevShipmentStatus;
    @JsonProperty("ReceivingBranch")
    private Long receivingBranch;
    @JsonProperty("ReferenceNumbersList")
    private List<ReferenceNumbersModel> referenceNumbersList;
    @JsonProperty("Route")
    private String route;
    @JsonProperty("RoutingsList")
    private List<RoutingsModel> routingsList;
    @JsonProperty("SalesAgent")
    private Long salesAgent;
    @JsonProperty("ServiceType")
    private String serviceType;
    @JsonProperty("ServicesList")
    private List<ServiceDetailsModel> servicesList;
    @JsonProperty("ShipmentCompletedBy")
    private String shipmentCompletedBy;
    @JsonProperty("ShipmentCompletedOn")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime shipmentCompletedOn;
    @JsonProperty("ShipmentCreatedOn")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime shipmentCreatedOn;
    @JsonProperty("ShipmentId")
    private String shipmentId;
    @JsonProperty("ShipmentType")
    private String shipmentType;
    @JsonProperty("Source")
    private String source;
    @JsonProperty("SourceTenantId")
    private Long sourceTenantId;
    @JsonProperty("Status")
    private Integer status;
    @JsonProperty("TransportMode")
    private String transportMode;
    @JsonProperty("TriangulationPartnerList")
    private List<Long> triangulationPartnerList;
    @JsonProperty("TriangulationPartner")
    private Long triangulationPartner;
    @JsonProperty("TruckDriverDetails")
    private List<TruckDriverDetailsModel> truckDriverDetails;
    @JsonProperty("Volume")
    @JsonSerialize(using = CustomVolumeValueSerializer.class)
    private BigDecimal volume;
    @JsonProperty("VolumeUnit")
    private String volumeUnit;
    @JsonProperty("VolumetricWeight")
    @JsonSerialize(using = CustomWeightValueSerializer.class)
    private BigDecimal volumetricWeight;
    @JsonProperty("VolumetricWeightUnit")
    private String volumetricWeightUnit;
    @JsonSerialize(using = CustomWeightValueSerializer.class)
    @JsonProperty("Weight")
    private BigDecimal weight;
    @JsonProperty("WeightUnit")
    private String weightUnit;
    @JsonProperty("JobStatus")
    private String jobStatus;
    @JsonProperty("GoodsValue")
    private BigDecimal goodsValue;
    @JsonProperty("GoodsValueCurrency")
    private String goodsValueCurrency;
    @JsonProperty("InsuranceValue")
    private BigDecimal insuranceValue;
    @JsonProperty("InsuranceValueCurrency")
    private String InsuranceValueCurrency;
    @JsonProperty("ConsolidationList")
    @JsonIgnoreProperties("ShipmentsList")
    private List<ConsolidationModel> consolidationList;
    @JsonProperty("ShipmentAddresses")
    private List<PartiesModel> shipmentAddresses;
    @JsonProperty("ShipmentContainers")
    private List<ShipmentContainers> shipmentContainersList;
    @JsonProperty("EntryRefNo")
    private String entryRefNo;
    @JsonProperty("Summary")
    private String summary;
    @JsonProperty("PackSummary")
    private String packSummary;
    @JsonProperty("Voyage")
    private String voyage;
    @JsonProperty("SecurityStatus")
    private String securityStatus;
    @JsonProperty("ContainsHazardous")
    private Boolean containsHazardous;
    private List<PickupDeliveryDetailsModel> pickupDeliveryDetailsInstructions;
    @JsonProperty("TransportInstructionId")
    private Long transportInstructionId;
    @JsonProperty("OrderManagementNumber")
    private String orderManagementNumber;
    @JsonProperty("OceanDGStatus")
    private OceanDGStatus oceanDGStatus;
    @JsonProperty("ShipmentOrders")
    private List<ShipmentOrderModel> shipmentOrders;
    private String document;
}

