package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.entity.enums.BookingSource;
import com.dpw.runner.shipment.services.entity.enums.BookingStatus;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import io.swagger.annotations.ApiModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import javax.validation.constraints.Digits;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Data
@Builder
@ApiModel("Customer Booking V3 Request Model")
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class CustomerBookingV3Request extends CommonRequest implements IRunnerRequest {
    private Long id;
    private BookingStatus bookingStatus;
    private String serviceMode;
    private PartiesRequest customer;
    private Boolean isCustomerFreeText;
    private PartiesRequest consignor;
    private Boolean isConsignorFreeText;
    private PartiesRequest consignee;
    private Boolean isConsigneeFreeText;
    private PartiesRequest notifyParty;
    private Boolean isNotifyPartyFreeText;
    private String customerEmail;
    private String bookingNumber;
    private LocalDateTime bookingDate;
    private String incoTerms;
    private CarrierDetailRequest carrierDetails;
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
    private String contractStatus;
    private String businessCode;
    private List<ContainerV3Request> containersList;
    private List<PackingV3Request> packingList;
    private List<RoutingsRequest> routingList;
    private List<BookingChargesRequest> bookingCharges;
    private List<FileRepoRequest> fileRepoList;
    private List<ReferenceNumbersRequest> referenceNumbersList;
    private Boolean isAutoWeightVolumeUpdate;
    private String fmcTlcId;
    private Boolean isPackageManual;
    private Boolean isConsignorAddressFreeText;
    private Boolean isConsigneeAddressFreeText;
    private Boolean isCustomerAddressFreeText;
    private Boolean isNotifyPartyAddressFreeText;
    private LocalDateTime shipmentCreatedDate;
    private LocalDateTime createdAt;
    private String clientCountry;
    private String consignorCountry;
    private String consigneeCountry;
    private String notifyPartyCountry;
    private String parentContractId;
    private String salesBranch;
    private String primarySalesAgentEmail;
    private String secondarySalesAgentEmail;
    private Boolean isNotifyConsigneeEqual;
    private Boolean isShipperClientEqual;
    private Boolean isConsigneeClientEqual;
    private String currentPartyForQuote;
    private BookingSource source;
    private UUID sourceGuid;
    private String orderManagementId;
    private String orderManagementNumber;
    private Boolean isDg;
    private String rejectionRemarks;
    private String shipmentReferenceNumber;
    private String integrationSource;
    private List<PartiesRequest> additionalParties;
    private String paymentTerms;
    private Boolean isReefer;
    private String incotermsLocation;
    private LocalDateTime cargoReadinessDate;
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
    private LocalDateTime pickupAtOriginDate;
    private LocalDateTime deliveryAtDestinationDate;
    @ExcludeTimeZone
    private LocalDateTime estimatedPickupAtOriginDate;
    private LocalDateTime estimatedDeliveryAtDestinationDate;
    private Long pickupAtOrigin;
    private Long deliveryAtDestination;
    private Long brokerageAtOrigin;
    private Long brokerageAtDestination;
    @ExcludeTimeZone
    private LocalDateTime brokerageAtOriginDate;
    @ExcludeTimeZone
    private LocalDateTime brokerageAtDestinationDate;
    @ExcludeTimeZone
    private LocalDateTime estimatedBrokerageAtOriginDate;
    @ExcludeTimeZone
    private LocalDateTime estimatedBrokerageAtDestinationDate;
    private LocalDateTime terminalCutoff;
    private LocalDateTime verifiedGrossMassCutoff;
    private LocalDateTime shippingInstructionCutoff;
    private LocalDateTime dgCutoff;
    private LocalDateTime reeferCutoff;
    private LocalDateTime earliestEmptyEquipmentPickUp;
    private LocalDateTime latestFullEquipmentDeliveredToCarrier;
    private LocalDateTime earliestDropOffFullEquipmentToCarrier;
    private LocalDateTime latestArrivalTime;
    private BigDecimal teuCount;
    private Long containers;
    private String packageType;
    private Long packages;
    private String description;
    private String marksnNumbers;
    private String additionalTerms;
    private LocalDateTime carrierDocCutOff;
    private LocalDateTime cargoReceiptWHCutOff;
    private LocalDateTime lastFreeDateCutOff;
    @Digits(integer = 3, fraction = 0, message = "Max 3 digits allowed for Number Of Free Days CutOff")
    private Integer numberOfFreeDaysCutOff;
}
