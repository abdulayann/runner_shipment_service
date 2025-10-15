package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.config.CustomVolumeValueSerializer;
import com.dpw.runner.shipment.services.config.CustomWeightValueSerializer;
import com.dpw.runner.shipment.services.config.DecimalPlaceValueSerializer;
import com.dpw.runner.shipment.services.entity.enums.AwbStatus;
import com.dpw.runner.shipment.services.entity.enums.FileStatus;
import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@Data
@Generated
@SuppressWarnings("java:S1948")
public abstract class BaseShipmentResponse implements IRunnerResponse {

    // Core Identifiers
    protected Long id;
    protected Integer tenantId;
    protected UUID guid;
    protected String houseBill;
    protected String transportMode;
    protected String direction;
    protected String shipmentType;
    protected Integer status;
    protected String shipmentStatus;
    protected String source;
    protected String jobType;
    protected String serviceType;
    protected String masterBill;
    protected String bookingReference;
    protected String consolRef;

    // Sales and Payment
    protected Long salesAgent;
    protected String paymentTerms;
    protected String incoterms;
    protected String shipmentId;
    protected String assignedTo;
    protected String additionalTerms;
    protected String goodsDescription;

    // Weight and Volume
    @JsonSerialize(using = CustomWeightValueSerializer.class)
    protected BigDecimal weight;
    protected String weightUnit;

    @JsonSerialize(using = CustomVolumeValueSerializer.class)
    protected BigDecimal volume;
    protected String volumeUnit;

    @JsonSerialize(using = CustomWeightValueSerializer.class)
    protected BigDecimal volumetricWeight;
    protected String volumetricWeightUnit;

    @JsonSerialize(using = DecimalPlaceValueSerializer.class)
    protected BigDecimal chargable;
    protected String chargeableUnit;

    protected Integer noOfPacks;
    protected String packsUnit;

    // Freight
    protected BigDecimal freightLocal;
    protected String freightLocalCurrency;
    protected BigDecimal freightOverseas;
    protected String freightOverseasCurrency;

    // Dates
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    protected LocalDateTime cargoDeliveryDate;

    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    protected LocalDateTime shipmentCreatedOn;

    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    protected LocalDateTime createdAt;

    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    protected LocalDateTime updatedAt;

    // Lock and Status
    protected String marksNum;
    protected Boolean isLocked;
    protected String lockedBy;
    protected Boolean isNotifyConsigneeEqual;
    protected String bookingType;
    protected String bookingNumber;

    // Branch and Partner Info
    protected Long sourceTenantId;
    protected List<TriangulationPartnerResponse> triangulationPartnerList;
    protected Long receivingBranch;
    protected String shipmentCompletedBy;

    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    protected LocalDateTime shipmentCompletedOn;

    // Carrier and Details
    protected CarrierDetailResponse carrierDetails;

    // Job and Container Info
    protected String jobStatus;
    protected Long containerCount;
    protected BigDecimal teuCount;
    protected Integer packCount;
    protected String department;

    // Goods Value
    protected BigDecimal goodsValue;
    protected String goodsValueCurrency;
    protected BigDecimal insuranceValue;
    protected String InsuranceValueCurrency;

    // Hazardous
    protected Boolean containsHazardous;

    // User Info
    protected String createdBy;

    // Contract and Location Info
    protected String contractId;
    protected String parentContractId;
    protected String contractType;
    protected String clientCountry;
    protected String consignorCountry;
    protected String consigneeCountry;
    protected String notifyPartyCountry;
    protected String salesBranch;
    protected String primarySalesAgentEmail;
    protected String secondarySalesAgentEmail;

    // Address IDs
    protected Long consigneeDpsAddressId;
    protected Long clientDpsAddressId;
    protected Long consignorDpsAddressId;
    protected Long notifyPartyDpsAddressId;

    // Booking Info
    protected Long shipmentCount;
    protected LocalDateTime bookingCreatedDate;
    protected String securityStatus;
    protected AwbStatus awbStatus;

    // Quote and Transfer
    protected String currentPartyForQuote;
    protected Boolean entityTransfer;
    protected String destinationSalesBranch;
    protected String destinationPrimarySalesAgentEmail;
    protected String destinationSecondarySalesAgentEmail;
    protected String destinationCurrentPartyForQuote;
    protected String destinationContractId;
    protected String destinationParentContractId;
    protected String destinationContractType;
    protected String updatedBy;

    // File and Action Status
    protected Integer pendingActionCount;
    protected FileStatus fileStatus;
    protected Boolean isReceivingBranchAdded;
    protected String transferStatus;
    protected Boolean isReceivingBranchManually;
    protected Boolean isTransferredToReceivingBranch;

    // Co-load Info
    protected String coLoadCarrierName;
    protected String coLoadBlNumber;

    // Booking Details
    protected UUID customerBookingGuid;
    protected Boolean isFrob;
    protected String consolidationNumber;
    protected Boolean isReefer;

    // Origin Branch
    protected Long originBranch;

    // Incoterms Location
    protected String incotermsLocation;

    // Control Info
    protected Boolean controlled;
    protected String controlledReferenceNumber;
    protected String partner;
    protected Long bookingAgent;
    protected String coLoadBkgNumber;

    // Pickup and Delivery
    protected String pickupAtOriginType;
    protected String deliveryAtDestinationType;
    protected String brokerageAtOriginType;
    protected String brokerageAtDestinationType;
    protected Long pickupAtOrigin;
    protected Long deliveryAtDestination;
    protected Long brokerageAtOrigin;
    protected Long brokerageAtDestination;

    // Cutoff Dates
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    protected LocalDateTime terminalCutoff;

    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    protected LocalDateTime verifiedGrossMassCutoff;

    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    protected LocalDateTime shippingInstructionCutoff;

    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    protected LocalDateTime dgCutoff;

    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    protected LocalDateTime reeferCutoff;

    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    protected LocalDateTime earliestEmptyEquipmentPickUp;

    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    protected LocalDateTime latestFullEquipmentDeliveredToCarrier;

    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    protected LocalDateTime earliestDropOffFullEquipmentToCarrier;

    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    protected LocalDateTime latestArrivalTime;

    // Container Assignment
    protected Long containerAssignedToShipmentCargo;

    // Borrowed Status
    protected Boolean isBorrowed;

    // SLAC
    protected Integer slac;

}
