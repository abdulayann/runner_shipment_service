package com.dpw.runner.shipment.services.dto.v3.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.request.PickupDeliveryDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.ReferenceNumbersRequest;
import com.dpw.runner.shipment.services.dto.request.TriangulationPartnerRequest;
import com.dpw.runner.shipment.services.dto.request.TruckDriverDetailsRequest;
import com.dpw.runner.shipment.services.entity.enums.ShipmentDetailsQuoteDateType;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.openapitools.jackson.nullable.JsonNullable;

import jakarta.validation.constraints.Size;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ShipmentPatchV3Request extends CommonRequest implements IRunnerRequest {
    private AdditionalDetailV3Request additionalDetails;
    @Size(max=25000, message = "max size is 25000 for additional terms")
    private JsonNullable<String> additionalTerms;
    private JsonNullable<String> assignedTo;
    private JsonNullable<Boolean> autoUpdateWtVol;
    @Size(max = 50, message = "Max size is 50 for bookingNumber")
    private JsonNullable<String> bookingNumber;
    private JsonNullable<String> bookingReference;
    private JsonNullable<String> bookingType;
    private JsonNullable<Boolean> cargoFinanceBooking;
    private CarrierPatchV3Request carrierDetails;
    private JsonNullable<BigDecimal> chargable;
    private JsonNullable<String> chargeableUnit;
    private PartiesRequest client;
    private PartiesRequest consignee;
    private PartiesRequest consigner;
    private JsonNullable<String> consolRef;
    private JsonNullable<Boolean> containerAutoWeightVolumeUpdate;
    private JsonNullable<String> direction;
    private JsonNullable<Long> documentationPartner;
    @Size(max = 3, message = "max size is 3 for entry detail")
    private JsonNullable<String> entryDetail;
    private JsonNullable<String> financeClosedBy;
    private JsonNullable<LocalDateTime> financeClosedOn;
    private JsonNullable<BigDecimal> freightLocal;
    private JsonNullable<String> freightLocalCurrency;
    private JsonNullable<BigDecimal> freightOverseas;
    private JsonNullable<String> freightOverseasCurrency;
    @Size(max = 25000, message = "max size is 25000 for goods description")
    private JsonNullable<String> goodsDescription;
    private JsonNullable<String> houseBill;
    private JsonNullable<Long> id;
    private JsonNullable<Integer> tenantId;
    private JsonNullable<String> incoterms;
    private JsonNullable<String> innerPackUnit;
    private JsonNullable<Integer> innerPacks;
    private JsonNullable<Boolean> intraBranch;
    private JsonNullable<Boolean> isDomestic;
    private JsonNullable<Boolean> isLocked;
    private JsonNullable<Boolean> isNotifyConsigneeEqual;
    private JsonNullable<Boolean> isShipperClientEqual;
    private JsonNullable<Boolean> isConsigneeClientEqual;
    private JsonNullable<Boolean> isShipmentReadOnly;
    private JsonNullable<String> jobType;
    private JsonNullable<String> lockedBy;
    @Size(max = 25000, message = "Max size is 25000 for marks and num")
    private JsonNullable<String> marksNum;
    @Size(max = 50, message = "Max size is 50 for masterBill")
    private JsonNullable<String> masterBill;
    private JsonNullable<BigDecimal> netWeight;
    private JsonNullable<String> netWeightUnit;
    private JsonNullable<Integer> noOfPacks;
    private JsonNullable<String> packsUnit;
    private JsonNullable<String> paymentTerms;
    private List<PickupDeliveryDetailsRequest> pickupDeliveryDetailsList;
    private JsonNullable<Integer> prevShipmentStatus;
    private JsonNullable<Long> receivingBranch;
    private List<ReferenceNumbersRequest> referenceNumbersList;
    private JsonNullable<String> route;
    private JsonNullable<Long> salesAgent;
    @Size(max=3, message = "max size is 3 for service Type")
    private JsonNullable<String> serviceType;
    private JsonNullable<String> shipmentCompletedBy;
    private JsonNullable<LocalDateTime> shipmentCompletedOn;
    @Size(max=50, message = "max size is 50 for shipment id")
    private JsonNullable<String> shipmentId;
    @Size(max=3, message = "max size is 3 for shipment type")
    private JsonNullable<String> shipmentType;
    private JsonNullable<String> source;
    private JsonNullable<Long> sourceTenantId;
    private JsonNullable<Integer> status;
    @Size(max=4, message = "max size is 4 for transport mode")
    private JsonNullable<String> transportMode;
    private JsonNullable<List<TriangulationPartnerRequest>> triangulationPartnerList;
    private JsonNullable<Long> triangulationPartner;
    private List<TruckDriverDetailsRequest> truckDriverDetails;
    private JsonNullable<BigDecimal> volume;
    @Size(max=10, message = "max size is 10 for volume unit")
    private JsonNullable<String> volumeUnit;
    private JsonNullable<BigDecimal> volumetricWeight;
    private JsonNullable<String> volumetricWeightUnit;
    private JsonNullable<BigDecimal> weight;
    private JsonNullable<String> weightUnit;
    @Size(max=3, message = "max size is 3 for job status")
    private JsonNullable<String> jobStatus;
    private JsonNullable<BigDecimal> goodsValue;
    @Size(max=3, message = "max size is 3 for goods value currency")
    private JsonNullable<String> goodsValueCurrency;
    private JsonNullable<BigDecimal> insuranceValue;
    @Size(max=3, message = "max size is 3 for insurance value currency")
    private JsonNullable<String> InsuranceValueCurrency;
    @Size(max=250, message = "max size is 250 for entry ref no")
    private JsonNullable<String> entryRefNo;
    private JsonNullable<String> flightStatus;
    private JsonNullable<Boolean> containsHazardous;
    @Size(max = 10, message = "Max size is 10 for fmcTlcId")
    private JsonNullable<String> fmcTlcId;
    private JsonNullable<String> commodity;
    private JsonNullable<Long> orderNumber;
    private JsonNullable<String> orderManagementId;
    private JsonNullable<String> orderManagementNumber;
    private JsonNullable<String> destinationParentContractId;
    private JsonNullable<LocalDateTime> quoteDate;
    private JsonNullable<ShipmentDetailsQuoteDateType> quoteDateType;
    private JsonNullable<Boolean> isNetworkFile;
    private JsonNullable<Boolean> isReceivingBranchManually;
    private JsonNullable<Boolean> isTransferredToReceivingBranch;
    private JsonNullable<Boolean> isReefer;
    @Size(max = 50, message = "Max size is 50 for incotermsLocation")
    private JsonNullable<String> incotermsLocation;
    private JsonNullable<LocalDateTime> cargoReadinessDate;
    private JsonNullable<Boolean> controlled;
    @Size(max = 50, message = "Max size is 50 for controlledReferenceNumber")
    private JsonNullable<String> controlledReferenceNumber;
    private JsonNullable<String> partner;
    private JsonNullable<Long> bookingAgent;
    @Size(max = 50, message = "Max size is 50 for coLoadBkgNumber")
    private JsonNullable<String> coLoadBkgNumber;
    private JsonNullable<String> pickupAtOriginType;
    private JsonNullable<String> deliveryAtDestinationType;
    private JsonNullable<String> brokerageAtOriginType;
    private JsonNullable<String> brokerageAtDestinationType;
    private JsonNullable<Long> pickupAtOrigin;
    private JsonNullable<Long> deliveryAtDestination;
    private JsonNullable<Long> brokerageAtOrigin;
    private JsonNullable<Long> brokerageAtDestination;
    @ExcludeTimeZone
    private JsonNullable<LocalDateTime> estimatedBrokerageAtOriginDate;
    @ExcludeTimeZone
    private JsonNullable<LocalDateTime> estimatedBrokerageAtDestinationDate;
    @ExcludeTimeZone
    private JsonNullable<LocalDateTime> brokerageAtOriginDate;
    @ExcludeTimeZone
    private JsonNullable<LocalDateTime> brokerageAtDestinationDate;
    private JsonNullable<LocalDateTime> terminalCutoff;
    private JsonNullable<LocalDateTime> verifiedGrossMassCutoff;
    private JsonNullable<LocalDateTime> shippingInstructionCutoff;
    private JsonNullable<LocalDateTime> dgCutoff;
    private JsonNullable<LocalDateTime> reeferCutoff;
    private JsonNullable<LocalDateTime> earliestEmptyEquipmentPickUp;
    private JsonNullable<LocalDateTime> latestFullEquipmentDeliveredToCarrier;
    private JsonNullable<LocalDateTime> earliestDropOffFullEquipmentToCarrier;
    private JsonNullable<LocalDateTime> latestArrivalTime;
    private JsonNullable<Boolean> isBorrowed;
    private JsonNullable<Long> originBranch;
    private JsonNullable<Integer> slac;
    private JsonNullable<Integer> dgPacksCount;
    private JsonNullable<String> dgPacksUnit;

}
