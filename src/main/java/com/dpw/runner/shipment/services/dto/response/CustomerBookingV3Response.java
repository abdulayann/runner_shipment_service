package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentSummaryWarningsResponse;
import com.dpw.runner.shipment.services.entity.enums.BookingSource;
import com.dpw.runner.shipment.services.entity.enums.BookingStatus;
import com.dpw.runner.shipment.services.entity.enums.MigrationStatus;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import io.swagger.annotations.ApiModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Data
@Builder
@ApiModel("Customer Booking V3 Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class CustomerBookingV3Response implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private String serviceMode;
    private BookingStatus bookingStatus;
    private PartiesResponse customer;
    private Boolean isCustomerFreeText;
    private PartiesResponse consignor;
    private Boolean isConsignorFreeText;
    private PartiesResponse consignee;
    private Boolean isConsigneeFreeText;
    private PartiesResponse notifyParty;
    private Boolean isNotifyPartyFreeText;
    private String customerEmail;
    private String bookingNumber;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime bookingDate;
    private String incoTerms;
    private CarrierDetailResponse carrierDetails;
    private String transportType;
    private String cargoType;
    private String direction;
    private Integer quantity;
    private String quantityUnit;
    private BigDecimal grossWeight;
    private String grossWeightUnit;
    private BigDecimal volume;
    private String volumeUnit;
    private BigDecimal weightVolume;
    private String weightVolumeUnit;
    private BigDecimal chargeable;
    private String chargeableUnit;
    private String contractId;
    private String createdBy;
    private String contractStatus;
    private BookingSource source;
    private String businessCode;
    private String shipmentId;
    private String shipmentEntityId;
    private String shipmentEntityIdV2;
    private String shipmentGuid;
    private List<ContainerResponse> containersList;
    private List<PackingResponse> packingList;
    private List<RoutingsResponse> routingList;
    private List<BookingChargesResponse> bookingCharges;
    private List<FileRepoResponse> fileRepoList;
    private List<ReferenceNumbersResponse> referenceNumbersList;
    private Map<String, String> masterData;
    private Boolean isAutoWeightVolumeUpdate;
    private String fmcTlcId;
    private BigDecimal totalRevenue;
    private Boolean isPackageManual;
    private Boolean isConsignorAddressFreeText;
    private Boolean isConsigneeAddressFreeText;
    private Boolean isCustomerAddressFreeText;
    private Boolean isNotifyPartyAddressFreeText;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime shipmentCreatedDate;
    private String clientCountry;
    private String consignorCountry;
    private String consigneeCountry;
    private String notifyPartyCountry;
    private String parentContractId;
    private String salesBranch;
    private Boolean isNotifyConsigneeEqual;
    private Boolean isShipperClientEqual;
    private Boolean isConsigneeClientEqual;
    private String primarySalesAgentEmail;
    private String secondarySalesAgentEmail;
    private Boolean isBillCreated;
    private String currentPartyForQuote;
    private UUID sourceGuid;
    private String orderManagementId;
    private String orderManagementNumber;
    private Boolean isDg;
    private Integer tenantId;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime createdAt;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime updatedAt;
    private String rejectionRemarks;
    private String shipmentReferenceNumber;
    private String integrationSource;
    private List<PartiesResponse> additionalParties;
    private String paymentTerms;
    private Boolean isReefer;
    private String incotermsLocation;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime cargoReadinessDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime cargoDeliveryDate;
    private Boolean controlled;
    private String controlledReferenceNumber;
    private String partner;
    private Long bookingAgent;
    private String coLoadCarrierName;
    private String partnerBkgNumber;
    private String partnerBLOrAWBNumber;
    private String carrierBookingNumber;
    private String pickupAtOriginType;
    private String deliveryAtDestinationType;
    private String brokerageAtOriginType;
    private String brokerageAtDestinationType;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime pickupAtOriginDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime deliveryAtDestinationDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime estimatedPickupAtOriginDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime estimatedDeliveryAtDestinationDate;
    private Long pickupAtOrigin;
    private Long deliveryAtDestination;
    private Long brokerageAtOrigin;
    private Long brokerageAtDestination;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime brokerageAtOriginDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime brokerageAtDestinationDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime estimatedBrokerageAtOriginDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime estimatedBrokerageAtDestinationDate;
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
    private BigDecimal teuCount;
    private Long containers;
    private String packageType;
    private Long packages;
    private String description;
    private String marksnNumbers;
    private String additionalTerms;
    private Boolean isVolumeEditable = Boolean.FALSE;
    private Boolean isCargoSummaryEditable = Boolean.FALSE;
    private Boolean isWeightEditable = Boolean.FALSE;
    private ShipmentSummaryWarningsResponse shipmentSummaryWarningsResponse;
    private MigrationStatus migrationStatus;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime carrierDocCutOff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime cargoReceiptWHCutOff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime lastFreeDateCutOff;
    private Integer numberOfFreeDaysCutOff;
}
