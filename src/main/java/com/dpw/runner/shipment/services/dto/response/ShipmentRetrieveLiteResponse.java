package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.enums.TransportInfoStatus;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.config.CustomVolumeValueSerializer;
import com.dpw.runner.shipment.services.config.CustomWeightValueSerializer;
import com.dpw.runner.shipment.services.config.DecimalPlaceValueSerializer;
import com.dpw.runner.shipment.services.entity.enums.AwbStatus;
import com.dpw.runner.shipment.services.entity.enums.CustomerCategoryRates;
import com.dpw.runner.shipment.services.entity.enums.DateBehaviorType;
import com.dpw.runner.shipment.services.entity.enums.FileStatus;
import com.dpw.runner.shipment.services.entity.enums.OceanDGStatus;
import com.dpw.runner.shipment.services.entity.enums.ShipmentPackStatus;
import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Builder
@NoArgsConstructor
@AllArgsConstructor
@Data
@Generated
@SuppressWarnings("java:S1948")
public class ShipmentRetrieveLiteResponse implements IRunnerResponse {
    private Long id;
    private Integer tenantId;
    private UUID guid;
    private String houseBill;
    private String transportMode;
    private String direction;
    private String shipmentType;
    private Integer status;
    private String shipmentStatus;
    private String source;
    private String jobType;
    private String serviceType;
    private String masterBill;
    private Long bookingId;
    private String bookingReference;
    private String consolRef;
    private Long salesAgent;
    private String paymentTerms;
    private String incoterms;
    private String shipmentId;
    private Boolean isDomestic;
    private String assignedTo;
    private String additionalTerms;
    private String goodsDescription;
    @JsonSerialize(using = CustomWeightValueSerializer.class)
    private BigDecimal weight;
    private String weightUnit;
    @JsonSerialize(using = CustomVolumeValueSerializer.class)
    private BigDecimal volume;
    private String volumeUnit;
    @JsonSerialize(using = CustomWeightValueSerializer.class)
    private BigDecimal volumetricWeight;
    private String volumetricWeightUnit;
    @JsonSerialize(using = DecimalPlaceValueSerializer.class)
    private BigDecimal chargable;
    private String chargeableUnit;
    @JsonSerialize(using = CustomWeightValueSerializer.class)
    private BigDecimal netWeight;
    private String netWeightUnit;
    private Integer noOfPacks;
    private String packsUnit;
    private Integer innerPacks;
    private String innerPackUnit;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime cargoReadyDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime cargoDeliveryDate;
    private BigDecimal freightLocal;
    private String freightLocalCurrency;
    private BigDecimal freightOverseas;
    private String freightOverseasCurrency;
    private Boolean autoUpdateWtVol;
    private Boolean containerAutoWeightVolumeUpdate;
    private String marksNum;
    private String entryDetail;
    private Boolean isLocked;
    private String lockedBy;
    private Boolean isNotifyConsigneeEqual;
    private String bookingType;
    private Boolean cargoFinanceBooking = Boolean.FALSE;
    private String bookingNumber;
    private String route;
    private Long sourceTenantId;
    private Long documentationPartner;
    private List<TriangulationPartnerResponse> triangulationPartnerList;
    private Long triangulationPartner;
    private Long receivingBranch;
    private Boolean intraBranch = Boolean.FALSE;
    private Integer prevShipmentStatus;
    @JsonProperty("isShipmentReadOnly")
    private Boolean isShipmentReadOnly = Boolean.FALSE;
    private String shipmentCompletedBy;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime shipmentCompletedOn;
    private String financeClosedBy;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime financeClosedOn;
    private PartiesResponse client;
    private PartiesResponse consigner;
    private PartiesResponse consignee;
    private AdditionalDetailResponse additionalDetails;
    private CarrierDetailResponse carrierDetails;
    private PickupDeliveryDetailsResponse deliveryDetails;
    private List<ReferenceNumbersResponse> referenceNumbersList;
    private List<TruckDriverDetailsResponse> truckDriverDetails;
    private String jobStatus;
    private BigDecimal goodsValue;
    private String goodsValueCurrency;
    private BigDecimal insuranceValue;
    private String InsuranceValueCurrency;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime shipmentCreatedOn;
    private String entryRefNo;
    private List<PartiesResponse> shipmentAddresses;
    private String flightStatus;
    private Boolean containsHazardous;
    private String fmcTlcId;
    private String commodity;
    private Long orderNumber;
    private String orderManagementId;
    private String orderManagementNumber;
    private String createdBy;
    private Map<String, String> textData;
    private Map<String, Long> containerData;
    private Long containerCount;
    private BigDecimal teuCount;
    private Integer packCount;
    private String department;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime createdAt;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime updatedAt;
    private CustomerCategoryRates customerCategory;
    private String contractId;
    private String parentContractId;
    private String contractType;
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
    private List<String> implicationList;
    private Long shipmentCount;
    private LocalDateTime bookingCreatedDate;
    private String securityStatus;
    private AwbStatus awbStatus;

    private String currentPartyForQuote;
    private Boolean entityTransfer;
    private String destinationSalesBranch;
    private String destinationPrimarySalesAgentEmail;
    private String destinationSecondarySalesAgentEmail;
    private String destinationCurrentPartyForQuote;
    private String destinationContractId;
    private String destinationContractType;
    private String updatedBy;
    private DateBehaviorType dateType;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime shipmentGateInDate;
    private ShipmentPackStatus shipmentPackStatus;
    private Integer pendingActionCount;
    private FileStatus fileStatus;
    private Boolean isReceivingBranchAdded;
    private OceanDGStatus oceanDGStatus;
    private Boolean syncRoutingFromConsolidation;
    private Boolean isNetworkFile;
    private String transferStatus;
    private Boolean isReceivingBranchManually;
    private Boolean isTransferredToReceivingBranch;
    private Boolean b2b;

    private Boolean isCoLoadEnabled;
    private String coLoadCarrierName;
    private String coLoadBlNumber;

    private String issuingCarrierName;
    private String oceanBlNumber;
    private UUID customerBookingGuid;
    private Boolean isFrob;
    private String consolidationNumber;
    private Boolean isReefer;
    private Boolean isPacksAvailable = Boolean.FALSE;
    private Long originBranch;
    private String incotermsLocation;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime cargoReadinessDate;
    private Boolean controlled;
    private String controlledReferenceNumber;
    private String partner;
    private Long bookingAgent;
    private String coLoadBkgNumber;
    private String pickupAtOriginType;
    private String deliveryAtDestinationType;
    private String brokerageAtOriginType;
    private String brokerageAtDestinationType;
    private Long pickupAtOrigin;
    private Long deliveryAtDestination;
    private Long brokerageAtOrigin;
    private Long brokerageAtDestination;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime brokerageAtOriginDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime brokerageAtDestinationDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime terminalCutoff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime verifiedGrossMassCutoff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime shippingInstructionCutoff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime dgCutoff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime reeferCutoff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime earliestEmptyEquipmentPickUp;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime latestFullEquipmentDeliveredToCarrier;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime earliestDropOffFullEquipmentToCarrier;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime latestArrivalTime;
    private Boolean isBorrowed;
    private Long containerAssignedToShipmentCargo;
    private Long consolidationId;
    private String consolBookingNumber;
    private Boolean isMainCarriageAvailable = Boolean.FALSE;
    private Integer dgPacksCount;
    private String dgPacksUnit;
    private Boolean isEmptyWeightPackAvailable = Boolean.FALSE;
    private Boolean isInterBranchConsoleAttached;
    private TransportInfoStatus transportInfoStatus;
    private String transportInfoStatusMessage;
    private Boolean isVesselVoyageOrCarrierFlightNumberAvailable = Boolean.FALSE;
    private Integer slac;
}
