package com.dpw.runner.shipment.services.dto.v3.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerIdDltReq;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.entity.enums.*;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import com.dpw.runner.shipment.services.utils.TrimStringDeserializer;
import com.dpw.runner.shipment.services.validator.annotations.ValidCargoDeliveryDate;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.Size;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.UUID;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ValidCargoDeliveryDate
@SuppressWarnings("java:S6539")
public class ShipmentEtV3Request extends CommonRequest implements IRunnerRequest {
    private AdditionalDetailV3Request additionalDetails;
    @Size(max=2048, message = "max size is 2048 for additional terms")
    private String additionalTerms;
    private String assignedTo;
    private Boolean autoUpdateWtVol;
    @Size(max = 50, message = "Max size is 50 for bookingNumber")
    private String bookingNumber;
    @JsonDeserialize(using = TrimStringDeserializer.class)
    private String bookingReference;
    private String bookingType;
    private Boolean cargoFinanceBooking;
    private CarrierDetailRequest carrierDetails;
    private BigDecimal chargable;
    private Boolean isChargableEditable;
    private String chargeableUnit;
    private PartiesRequest client;
    private PartiesRequest consignee;
    private PartiesRequest consigner;
    private String consolRef;
    private Boolean containerAutoWeightVolumeUpdate;
    private String direction;
    private Long documentationPartner;
    @Size(max = 3, message = "max size is 3 for entry detail")
    private String entryDetail;
    private String financeClosedBy;
    private LocalDateTime financeClosedOn;
    private BigDecimal freightLocal;
    private String freightLocalCurrency;
    private BigDecimal freightOverseas;
    private String freightOverseasCurrency;
    @Size(max = 50000, message = "max size is 50000 for goods description")
    private String goodsDescription;
    @JsonDeserialize(using = TrimStringDeserializer.class)
    private String houseBill;
    private Long id;
    private Integer tenantId;
    private String incoterms;
    private String innerPackUnit;
    private Integer innerPacks;
    private Boolean intraBranch;
    private Boolean isDomestic;
    private Boolean isLocked;
    private Boolean isNotifyConsigneeEqual;
    private Boolean isShipmentReadOnly;
    private String jobType;
    private String lockedBy;
    @Size(max = 50000, message = "Max size is 50000 for marks and num")
    private String marksNum;
    @JsonDeserialize(using = TrimStringDeserializer.class)
    @Size(max = 50, message = "Max size is 50 for masterBill")
    private String masterBill;
    private BigDecimal netWeight;
    private String netWeightUnit;
    private Integer noOfPacks;
    private String packsUnit;
    private String paymentTerms;
    private PickupDeliveryDetailsRequest pickupDetails;
    private PickupDeliveryDetailsRequest deliveryDetails;
    private Integer prevShipmentStatus;
    private Long receivingBranch;
    private List<ReferenceNumbersRequest> referenceNumbersList;
    private String route;
    private Long salesAgent;
    @Size(max=3, message = "max size is 3 for service Type")
    private String serviceType;
    private String shipmentCompletedBy;
    private LocalDateTime shipmentCompletedOn;
    private LocalDateTime shipmentCreatedOn;
    @Size(max=50, message = "max size is 50 for shipment id")
    private String shipmentId;
    @Size(max=3, message = "max size is 3 for shipment type")
    private String shipmentType;
    private String source;
    private Long sourceTenantId;
    private Integer status;
    @Size(max=4, message = "max size is 4 for transport mode")
    private String transportMode;
    private List<TriangulationPartnerRequest> triangulationPartnerList;
    private Long triangulationPartner;
    private List<TruckDriverDetailsRequest> truckDriverDetails;
    private BigDecimal volume;
    @Size(max=10, message = "max size is 10 for volume unit")
    private String volumeUnit;
    private BigDecimal volumetricWeight;
    private String volumetricWeightUnit;
    private BigDecimal weight;
    private String weightUnit;
    @Size(max=3, message = "max size is 3 for job status")
    private String jobStatus;
    public BigDecimal goodsValue;
    @Size(max=3, message = "max size is 3 for goods value currency")
    public String goodsValueCurrency;
    public BigDecimal insuranceValue;
    @Size(max=3, message = "max size is 3 for insurance value currency")
    public String InsuranceValueCurrency;
    @Size(max=250, message = "max size is 250 for entry ref no")
    public String entryRefNo;
    private List<PartiesRequest> shipmentAddresses;
    private String flightStatus;
    private Boolean containsHazardous;
    @Size(max = 10, message = "Max size is 10 for fmcTlcId")
    private String fmcTlcId;
    private String commodity;
    private Long orderNumber;
    private String orderManagementId;
    private String orderManagementNumber;
    private CustomerCategoryRates customerCategory;
    @Size(max=64, message = "max size is 64 for contract id")
    private String contractId;
    private String parentContractId;
    @Size(max=64, message = "max size is 64 for contract type")
    private String contractType;
    private Boolean replaceConsoleRoute;
    private Boolean createMainLegRoute;
    private String clientCountry;
    private String consignorCountry;
    private String consigneeCountry;
    private String notifyPartyCountry;
    private String salesBranch;
    private String primarySalesAgentEmail;
    private String secondarySalesAgentEmail;
    private UUID sourceGuid;
    private UUID clonedGuid;
    private Long consigneeDpsAddressId;
    private Long clientDpsAddressId;
    private Long consignorDpsAddressId;
    private Long notifyPartyDpsAddressId;
    private Boolean autoCreateConsole;
    private LocalDateTime bookingCreatedDate;
    private String securityStatus;
    private String currentPartyForQuote;
    private String destinationSalesBranch;
    private String destinationPrimarySalesAgentEmail;
    private String destinationSecondarySalesAgentEmail;
    private String destinationCurrentPartyForQuote;
    private String destinationContractId;
    private String destinationContractType;
    private Boolean isAutoSellRequired;

    private UpstreamDateUpdateRequest dateUpdateRequest;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;
    private DateBehaviorType dateType;
    private LocalDateTime shipmentGateInDate;
    private ShipmentPackStatus shipmentPackStatus;
    @Size(max=32, message = "max size is 32 for department")
    private String department;
    private OceanDGStatus oceanDGStatus;
    private Boolean isRefeer;

    // Consolidation achieved values
    private AchievedQuantitiesRequest consolidationAchievedQuantities;

    private LocalDateTime cargoReadyDate;
    private LocalDateTime cargoDeliveryDate;
    private Boolean isReceivingBranchAdded;
    private FileStatus fileStatus;
    private Boolean syncRoutingFromConsolidation;
    private Boolean isNetworkFile;
    private Boolean isReceivingBranchManually;
    private Boolean isTransferredToReceivingBranch;
    private String shipmentStatus;
    private Boolean b2b;

    private Boolean isCoLoadEnabled;
    @Size(max = 64, message = "max Size is 64 for coLoadCarrierName")
    private String coLoadCarrierName;
    @Size(max = 50, message = "Max size is 50 for coLoadBlNumber")
    private String coLoadBlNumber;
    @Size(max = 64, message = "max Size is 64 for issuingCarrierName")
    private String issuingCarrierName;
    @Size(max = 64, message = "max Size is 64 for oceanBlNumber")
    private String oceanBlNumber;
    private UUID customerBookingGuid;
    private Boolean isFrob;
    private Boolean isReefer;
    @Size(max = 50, message = "Max size is 50 for incotermsLocation")
    private String incotermsLocation;
    private LocalDateTime cargoReadinessDate;
    private Boolean controlled;
    @Size(max = 50, message = "Max size is 50 for controlledReferenceNumber")
    private String controlledReferenceNumber;
    private String partner;
    private Long bookingAgent;
    @Size(max = 50, message = "Max size is 50 for coLoadBkgNumber")
    private String coLoadBkgNumber;
    private String pickupAtOriginType;
    private String deliveryAtDestinationType;
    private String brokerageAtOriginType;
    private String brokerageAtDestinationType;
    private Long pickupAtOrigin;
    private Long deliveryAtDestination;
    private Long brokerageAtOrigin;
    private Long brokerageAtDestination;
    @ExcludeTimeZone
    private LocalDateTime brokerageAtOriginDate;
    @ExcludeTimeZone
    private LocalDateTime brokerageAtDestinationDate;
    private LocalDateTime terminalCutoff;
    private LocalDateTime verifiedGrossMassCutoff;
    private LocalDateTime shippingInstructionCutoff;
    private LocalDateTime dgCutoff;
    private LocalDateTime reeferCutoff;
    private LocalDateTime earliestEmptyEquipmentPickUp;
    private LocalDateTime latestFullEquipmentDeliveredToCarrier;
    private LocalDateTime earliestDropOffFullEquipmentToCarrier;
    private LocalDateTime latestArrivalTime;
    private Long containerAssignedToShipmentCargo;
    private Boolean isBorrowed;
    private Long originBranch;

    private Set<ConsolidationDetailsRequest> consolidationList;
    private List<PackingV3Request> packingList;
    private Set<ContainerRequest> containersList;
    private List<BookingCarriageRequest> bookingCarriagesList;
    private List<ELDetailsRequest> elDetailsList;
    private List<EventsRequest> eventsList = new ArrayList<>();
    private List<NotesRequest> notesList;
    private List<ServiceDetailsRequest> servicesList;
    private List<PickupDeliveryDetailsRequest> pickupDeliveryDetailsInstructions;
    private List<ShipmentOrderRequest> shipmentOrders;
    private List<ContainerIdDltReq> deletedContainerIds;
    private List<RoutingsRequest> routingsList;

}
