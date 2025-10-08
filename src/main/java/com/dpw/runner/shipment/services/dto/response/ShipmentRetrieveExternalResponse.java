package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.enums.TransportInfoStatus;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.config.CustomVolumeValueSerializer;
import com.dpw.runner.shipment.services.config.CustomWeightValueSerializer;
import com.dpw.runner.shipment.services.config.DecimalPlaceValueSerializer;
import com.dpw.runner.shipment.services.entity.Notes;
import com.dpw.runner.shipment.services.entity.enums.*;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Generated
@ToString(onlyExplicitlyIncluded = true)
public class ShipmentRetrieveExternalResponse implements IRunnerResponse {
    private Long id;
    private Integer tenantId;
    private UUID guid;
    private String houseBill;
    private String direction;
    private Integer status;
    private String shipmentStatus;
    private String serviceType;
    private String masterBill;
    private String consolRef;
    private Long salesAgent;
    private String paymentTerms;
    private String incoterms;
    private String shipmentId;
    private String assignedTo;
    private String additionalTerms;
    private String goodsDescription;

    @JsonSerialize(using = CustomWeightValueSerializer.class)
    private BigDecimal volumetricWeight;
    private String volumetricWeightUnit;
    @JsonSerialize(using = CustomVolumeValueSerializer.class)
    private BigDecimal volume;
    private String volumeUnit;
    @JsonSerialize(using = CustomWeightValueSerializer.class)
    private BigDecimal weight;
    private String weightUnit;

    @JsonSerialize(using = DecimalPlaceValueSerializer.class)
    private BigDecimal chargable;
    private String chargeableUnit;
    private Integer noOfPacks;
    private String packsUnit;
    private BigDecimal freightOverseas;
    private String freightOverseasCurrency;
    private BigDecimal freightLocal;
    private String freightLocalCurrency;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime cargoReadyDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime cargoDeliveryDate;
    private String marksNum;
    private String lockedBy;
    private Boolean isLocked;
    private Boolean isNotifyConsigneeEqual;
    private String bookingNumber;
    private String bookingType;
    private Long sourceTenantId;
    private List<TriangulationPartnerResponse> triangulationPartnerList;
    private Long receivingBranch;
    private String shipmentCompletedBy;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime shipmentCompletedOn;
    private PartiesExternalResponse consignee;
    private PartiesExternalResponse consigner;
    private PartiesExternalResponse client;
    private AdditionalDetailExternalResponse additionalDetails;
    private CarrierDetailResponse carrierDetails;
    private List<TruckDriverDetailsResponse> truckDriverDetails;
    private String jobStatus;
    private BigDecimal goodsValue;
    private List<ReferenceNumbersResponse> referenceNumbersList;
    private String goodsValueCurrency;
    private BigDecimal insuranceValue;
    private String InsuranceValueCurrency;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime shipmentCreatedOn;
    private List<PartiesExternalResponse> shipmentAddresses;
    private Boolean containsHazardous;
    private String fmcTlcId;
    private String createdBy;
    private BigDecimal teuCount;
    private Map<String, Long> containerData;
    private Long containerCount;
    private Integer packCount;
    private String department;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime updatedAt;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime createdAt;
    private String contractId;
    private Map<String, String> textData;
    private String parentContractId;
    private String contractType;
    private String clientCountry;
    private String consignorCountry;
    private String consigneeCountry;
    private String notifyPartyCountry;
    private String secondarySalesAgentEmail;
    private String primarySalesAgentEmail;
    private String salesBranch;
    private UUID sourceGuid;
    private Long consignorDpsAddressId;
    private Long consigneeDpsAddressId;
    private Long clientDpsAddressId;
    private Long notifyPartyDpsAddressId;
    private Long shipmentCount;
    private LocalDateTime bookingCreatedDate;
    private String securityStatus;
    private AwbStatus awbStatus;
    private List<String> implicationList;

    private String currentPartyForQuote;
    private Boolean entityTransfer;
    private String destinationPrimarySalesAgentEmail;
    private String destinationSalesBranch;
    private String destinationCurrentPartyForQuote;
    private String destinationSecondarySalesAgentEmail;
    private String destinationContractId;
    private String destinationParentContractId;
    private String updatedBy;
    private String destinationContractType;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime quoteDate;
    private ShipmentDetailsQuoteDateType quoteDateType;
    private Integer pendingActionCount;
    private FileStatus fileStatus;
    private Boolean isReceivingBranchAdded;
    private OceanDGStatus oceanDGStatus;
    private String transferStatus;
    private Boolean isReceivingBranchManually;
    private Boolean isTransferredToReceivingBranch;

    private String coLoadCarrierName;
    private String coLoadBlNumber;
    private String transportMode;
    private String shipmentType;
    private String source;
    private String jobType;
    private Long bookingId;
    private String bookingReference;

    private UUID customerBookingGuid;
    private Boolean isFrob;
    private String consolidationNumber;
    private Boolean isReefer;
    private Boolean isPacksAvailable = Boolean.FALSE;
    private Long originBranch;
    private String incotermsLocation;
    private String controlledReferenceNumber;
    private Boolean controlled;
    private String partner;
    private Long bookingAgent;
    private String coLoadBkgNumber;
    private String brokerageAtOriginType;
    private String brokerageAtDestinationType;
    private Long pickupAtOrigin;
    private Long deliveryAtDestination;
    private Long brokerageAtOrigin;
    private Long brokerageAtDestination;
    private String pickupAtOriginType;
    private String deliveryAtDestinationType;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime brokerageAtDestinationDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime brokerageAtOriginDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime verifiedGrossMassCutoff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime shippingInstructionCutoff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime reeferCutoff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime earliestEmptyEquipmentPickUp;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime earliestDropOffFullEquipmentToCarrier;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime latestArrivalTime;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime terminalCutoff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime dgCutoff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime latestFullEquipmentDeliveredToCarrier;
    private Long containerAssignedToShipmentCargo;
    private Long consolidationId;
    private Boolean isMainCarriageAvailable = Boolean.FALSE;
    private Integer dgPacksCount;
    private Boolean isEmptyWeightPackAvailable = Boolean.FALSE;
    private Boolean isInterBranchConsoleAttached;
    private TransportInfoStatus transportInfoStatus;
    private String transportInfoStatusMessage;
    private Integer slac;
    private Boolean isVolumeEditable = Boolean.FALSE;
    private List<RoutingsLiteResponse> routingsLiteResponses;
    private Boolean isBorrowed;
    private String consolBookingNumber;
    private String dgPacksUnit;
    private Boolean isVesselVoyageOrCarrierFlightNumberAvailable = Boolean.FALSE;
    private Boolean isCargoSummaryEditable = Boolean.FALSE;

    private List<RoutingsResponse> routingsList;
    private List<PackingResponse> packingList;
    private List<EventsResponse> eventsList;
    private Set<ContainerResponse> containersList;
    private List<NotesResponse> customerBookingNotesList;
    private List<Notes> notesList;
    private Set<ConsolidationListIdExternalResponse> consolidationList;
}
