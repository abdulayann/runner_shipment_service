package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.entity.enums.BookingSource;
import com.dpw.runner.shipment.services.entity.enums.BookingStatus;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import io.swagger.annotations.ApiModel;
import lombok.*;

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
    private String rejectionRemarks;
    private LocalDateTime etd;
    private LocalDateTime eta;
    private Boolean isReefer;
    private String incotermsLocation;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
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
}
