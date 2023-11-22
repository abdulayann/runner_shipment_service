package com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel;

import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class ShipmentModel {
    @JsonProperty("additionalDetails")
    private AdditionalDetailModel additionalDetails;
    @JsonProperty("additionalTerms")
    private String additionalTerms;
    @JsonProperty("assignedTo")
    private String assignedTo;
    @JsonProperty("autoUpdateWtVol")
    private Boolean autoUpdateWtVol;
    @JsonProperty("bookingCarriagesList")
    private List<BookingCarriageModel> bookingCarriagesList;
    @JsonProperty("bookingNumber")
    private String bookingNumber;
    @JsonProperty("bookingReference")
    private String bookingReference;
    @JsonProperty("bookingType")
    private String bookingType;
    @JsonProperty("cargoFinanceBooking")
    private Boolean cargoFinanceBooking;
    @JsonProperty("carrierDetails")
    private CarrierDetailModel carrierDetails;
    @JsonProperty("chargable")
    private BigDecimal chargable;
    @JsonProperty("chargeableUnit")
    private String chargeableUnit;
    @JsonProperty("client")
    private PartiesModel client;
    @JsonProperty("consignee")
    private PartiesModel consignee;
    @JsonProperty("consigner")
    private PartiesModel consigner;
    @JsonProperty("consolRef")
    private String consolRef;
    @JsonProperty("containerAutoWeightVolumeUpdate")
    private Boolean containerAutoWeightVolumeUpdate;
    @JsonProperty("containersList")
    private List<ContainerModel> containersList = new ArrayList<>();
    @JsonProperty("direction")
    private String direction;
    @JsonProperty("documentationPartner")
    private Long documentationPartner;
    @JsonProperty("elDetailsList")
    private List<ELDetailsModel> elDetailsList;
    @JsonProperty("entryDetail")
    private String entryDetail;
    @JsonProperty("eventsList")
    private List<EventsModel> eventsList;
    @JsonProperty("fileRepoList")
    private List<FileRepoModel> fileRepoList;
    @JsonProperty("financeClosedBy")
    private String financeClosedBy;
    @JsonProperty("financeClosedOn")
    private LocalDateTime financeClosedOn;
    @JsonProperty("freightLocal")
    private Integer freightLocal;
    @JsonProperty("freightLocalCurrency")
    private String freightLocalCurrency;
    @JsonProperty("freightOverseas")
    private Integer freightOverseas;
    @JsonProperty("freightOverseasCurrency")
    private String freightOverseasCurrency;
    @JsonProperty("goodsDescription")
    private String goodsDescription;
    @JsonProperty("houseBill")
    private String houseBill;
    @JsonProperty("id")
    private Long id;
    @JsonProperty("incoterms")
    private String incoterms;
    @JsonProperty("innerPackUnit")
    private String innerPackUnit;
    @JsonProperty("innerPacks")
    private Integer innerPacks;
    @JsonProperty("intraBranch")
    private Boolean intraBranch;
    @JsonProperty("isDomestic")
    private Boolean isDomestic;
    @JsonProperty("isLocked")
    private Boolean isLocked;
    @JsonProperty("isNotifyConsigneeEqual")
    private Boolean isNotifyConsigneeEqual;
    @JsonProperty("isShipmentReadOnly")
    private Boolean isShipmentReadOnly;
    @JsonProperty("jobType")
    private String jobType;
    @JsonProperty("jobsList")
    private List<JobModel> jobsList;
    @JsonProperty("lockedBy")
    private String lockedBy;
    @JsonProperty("marksNum")
    private String marksNum;
    @JsonProperty("masterBill")
    private String masterBill;
    @JsonProperty("netWeight")
    private BigDecimal netWeight;
    @JsonProperty("netWeightUnit")
    private String netWeightUnit;
    @JsonProperty("noOfPacks")
    private Integer noOfPacks;
    @JsonProperty("notesList")
    private List<NotesModel> notesList;
    @JsonProperty("packingList")
    private List<PackingModel> packingList;
    @JsonProperty("packsUnit")
    private String packsUnit;
    @JsonProperty("paymentTerms")
    private String paymentTerms;
    @JsonProperty("pickupDetails")
    private PickupDeliveryDetailsModel pickupDetails;
    @JsonProperty("deliveryDetails")
    private PickupDeliveryDetailsModel deliveryDetails;
    @JsonProperty("prevShipmentStatus")
    private Integer prevShipmentStatus;
    @JsonProperty("receivingBranch")
    private Long receivingBranch;
    @JsonProperty("referenceNumbersList")
    private List<ReferenceNumbersModel> referenceNumbersList;
    @JsonProperty("route")
    private String route;
    @JsonProperty("routingsList")
    private List<RoutingsModel> routingsList;
    @JsonProperty("salesAgent")
    private Long salesAgent;
    @JsonProperty("serviceType")
    private String serviceType;
    @JsonProperty("servicesList")
    private List<ServiceDetailsModel> servicesList;
    @JsonProperty("shipmentCompletedBy")
    private String shipmentCompletedBy;
    @JsonProperty("shipmentCompletedOn")
    private LocalDateTime shipmentCompletedOn;
    @JsonProperty("shipmentCreatedOn")
    private LocalDateTime shipmentCreatedOn;
    @JsonProperty("shipmentId")
    private String shipmentId;
    @JsonProperty("shipmentType")
    private String shipmentType;
    @JsonProperty("source")
    private String source;
    @JsonProperty("sourceTenantId")
    private Long sourceTenantId;
    @JsonProperty("status")
    private Integer status;
    @JsonProperty("transportMode")
    private String transportMode;
    @JsonProperty("triangulationPartner")
    private Long triangulationPartner;
    @JsonProperty("truckDriverDetails")
    private List<TruckDriverDetailsModel> truckDriverDetails;
    @JsonProperty("volume")
    private BigDecimal volume;
    @JsonProperty("volumeUnit")
    private String volumeUnit;
    @JsonProperty("volumetricWeight")
    private BigDecimal volumetricWeight;
    @JsonProperty("volumetricWeightUnit")
    private String volumetricWeightUnit;
    @JsonProperty("weight")
    private BigDecimal weight;
    @JsonProperty("weightUnit")
    private String weightUnit;
    @JsonProperty("jobStatus")
    private String jobStatus;
    @JsonProperty("goodsValue")
    private BigDecimal goodsValue;
    @JsonProperty("goodsValueCurrency")
    private String goodsValueCurrency;
    @JsonProperty("insuranceValue")
    private BigDecimal insuranceValue;
    @JsonProperty("insuranceValueCurrency")
    private String InsuranceValueCurrency;
    @JsonProperty("consolidationList")
    @JsonIgnoreProperties("shipmentsList")
    private List<ConsolidationModel> consolidationList;
    @JsonProperty("shipmentAddresses")
    private List<PartiesModel> shipmentAddresses;
    @JsonProperty("shipmentContainers")
    private List<ShipmentContainers> shipmentContainersList;
    @JsonProperty("entryRefNo")
    public String entryRefNo;

}

