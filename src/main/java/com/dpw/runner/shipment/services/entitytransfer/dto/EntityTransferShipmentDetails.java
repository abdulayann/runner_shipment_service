package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerSummaryResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.entity.enums.DateBehaviorType;
import com.dpw.runner.shipment.services.entity.enums.ShipmentPackStatus;
import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class EntityTransferShipmentDetails implements IEntityTranferBaseEntity, Serializable {
    private UUID guid;
    private String houseBill;
    private String transportMode;
    private String direction;
    private String shipmentType;
    private Integer status;
    private String source;
    private String jobType;
    private String serviceType;
    private String masterBill;
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
    private BigDecimal weight;
    private String weightUnit;
    private BigDecimal volume;
    private String volumeUnit;
    private BigDecimal volumetricWeight;
    private String volumetricWeightUnit;
    private BigDecimal chargable;
    private String chargeableUnit;
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
    private boolean cargoFinanceBooking;
    private String bookingNumber;
    private String route;
    private Long sourceTenantId;
    private Long documentationPartner;
    private List<Long> triangulationPartnerList;
    private Long triangulationPartner;
    private Long receivingBranch;
    private boolean intraBranch;
    private Integer prevShipmentStatus;
    @JsonProperty("isShipmentReadOnly")
    private boolean isShipmentReadOnly;
    private String shipmentCompletedBy;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime shipmentCompletedOn;
    private String financeClosedBy;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime financeClosedOn;
    private EntityTransferParties client;
    private EntityTransferParties consigner;
    private EntityTransferParties consignee;
    private EntityTransferAdditionalDetails additionalDetails;
    private EntityTransferCarrierDetails carrierDetails;
    private List<EntityTransferRoutings> routingsList;
    private List<EntityTransferReferenceNumbers> referenceNumbersList;
    private List<EntityTransferPacking> packingList;
    private List<EntityTransferBookingCarriage> bookingCarriagesList;
    private List<EntityTransferContainers> containersList;
    private List<EntityTransferNotes> notesList;

    private List<EntityTransferServiceDetails> servicesList;

    private String jobStatus;
    private BigDecimal goodsValue;
    private String goodsValueCurrency;
    private BigDecimal insuranceValue;
    private String insuranceValueCurrency;
    private String entryRefNo;
    private List<EntityTransferParties> shipmentAddresses;
    private String flightStatus;
    private Boolean containsHazardous;
    private String fmcTlcId;
    private String commodity;
    private Long orderNumber;
    private String orderManagementId;
    private String orderManagementNumber;
    private ContainerSummaryResponse containerSummary;
    private PackSummaryResponse packSummary;
    private Map<String, String> textData;
    private Map<String, Long> containerData;

    private String contractId;
    private String contractType;
    private String clientCountry;
    private String consignorCountry;
    private String consigneeCountry;
    private String notifyPartyCountry;
    private String salesBranch;
    private String primarySalesAgentEmail;
    private String secondarySalesAgentEmail;
    private UUID clonedGuid;
    private Long consigneeDpsAddressId;
    private Long clientDpsAddressId;
    private Long consignorDpsAddressId;
    private Long notifyPartyDpsAddressId;
    private Long shipmentCount;
    private LocalDateTime bookingCreatedDate;
    private String securityStatus;

    private String currentPartyForQuote;
    private String destinationSalesBranch;
    private String destinationPrimarySalesAgentEmail;
    private String destinationSecondarySalesAgentEmail;
    private String destinationCurrentPartyForQuote;
    private String destinationContractId;
    private String destinationContractType;
    private DateBehaviorType dateType;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime shipmentGateInDate;
    private ShipmentPackStatus shipmentPackStatus;
    private Integer pendingActionCount;

    private String sourceBranchTenantName;

    private transient Map<String, Object> masterData;
    private Integer sendToBranch;

    private List<String> additionalDocs;
}