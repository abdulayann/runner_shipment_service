package com.dpw.runner.shipment.services.dto.v3.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.entity.enums.*;
import com.dpw.runner.shipment.services.utils.TrimStringDeserializer;
import com.dpw.runner.shipment.services.validator.annotations.ValidCargoDeliveryDate;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Column;
import javax.validation.constraints.Size;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ValidCargoDeliveryDate
@SuppressWarnings("java:S6539")
public class ShipmentV3Request extends CommonRequest implements IRunnerRequest {
    private AdditionalDetailRequest additionalDetails;
    private String additionalTerms;
    private String assignedTo;
    private Boolean autoUpdateWtVol;
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
    private String entryDetail;
    private String financeClosedBy;
    private LocalDateTime financeClosedOn;
    private BigDecimal freightLocal;
    private String freightLocalCurrency;
    private BigDecimal freightOverseas;
    private String freightOverseasCurrency;
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
    private String marksNum;
    @JsonDeserialize(using = TrimStringDeserializer.class)
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
    private String serviceType;
    private String shipmentCompletedBy;
    private LocalDateTime shipmentCompletedOn;
    private LocalDateTime shipmentCreatedOn;
    private String shipmentId;
    private String shipmentType;
    private String source;
    private Long sourceTenantId;
    private Integer status;
    private String transportMode;
    private List<TriangulationPartnerRequest> triangulationPartnerList;
    private Long triangulationPartner;
    private List<TruckDriverDetailsRequest> truckDriverDetails;
    private BigDecimal volume;
    private String volumeUnit;
    private BigDecimal volumetricWeight;
    private String volumetricWeightUnit;
    private BigDecimal weight;
    private String weightUnit;
    private String jobStatus;
    public BigDecimal goodsValue;
    public String goodsValueCurrency;
    public BigDecimal insuranceValue;
    public String InsuranceValueCurrency;
    public String entryRefNo;
    private List<PartiesRequest> shipmentAddresses;
    private String flightStatus;
    private Boolean containsHazardous;
    private String fmcTlcId;
    private String commodity;
    private Long orderNumber;
    private String orderManagementId;
    private String orderManagementNumber;
    private CustomerCategoryRates customerCategory;
    private String contractId;
    private String parentContractId;
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
    private String coLoadCarrierName;
    private String coLoadBlNumber;

    private String issuingCarrierName;
    private String oceanBlNumber;
    private UUID customerBookingGuid;
    private Boolean isFrob;
    private Boolean isReefer;
    private String incotermsLocation;
    private LocalDateTime cargoReadinessDate;
    private String controlled;
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
    private LocalDateTime brokerageAtOriginDate;
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

}
