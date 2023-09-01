package com.dpw.runner.shipment.services.dto.patchRequest;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.request.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.openapitools.jackson.nullable.JsonNullable;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ShipmentPatchRequest extends CommonRequest implements IRunnerRequest {
    private AdditionalDetailRequest additionalDetail;
    private JsonNullable<String> additionalTerms;
    private JsonNullable<String> assignedTo;
    private JsonNullable<Boolean> autoUpdateWtVol;
    private List<BookingCarriageRequest> bookingCarriagesList;
    private JsonNullable<String> bookingNumber;
    private JsonNullable<String> bookingReference;
    private JsonNullable<String> bookingType;
    private JsonNullable<Boolean> cargoFinanceBooking;
    private CarrierDetailRequest carrierDetails;
    private JsonNullable<BigDecimal> chargable;
    private JsonNullable<String> chargeableUnit;
    private PartiesRequest client;
    private PartiesRequest consignee;
    private PartiesRequest consigner;
    private JsonNullable<String> consolRef;
    private JsonNullable<Boolean> containerAutoWeightVolumeUpdate;
    private List<ContainerRequest> containersList;
    private JsonNullable<String> direction;
    private JsonNullable<Long> documentationPartner;
    private List<ELDetailsRequest> elDetailsList;
    private JsonNullable<String> entryDetail;
    private List<EventsRequest> eventsList;
    private List<FileRepoRequest> fileRepoList;
    private JsonNullable<String> financeClosedBy;
    private JsonNullable<LocalDateTime> financeClosedOn;
    private JsonNullable<Integer> freightLocal;
    private JsonNullable<String> freightLocalCurrency;
    private JsonNullable<Integer> freightOverseas;
    private JsonNullable<String> freightOverseasCurrency;
    private JsonNullable<String> goodsDescription;
    private JsonNullable<String> houseBill;
    private JsonNullable<Long> id;
    private JsonNullable<String> incoterms;
    private JsonNullable<String> innerPackUnit;
    private JsonNullable<Integer> innerPacks;
    private JsonNullable<Boolean> intraBranch;
    private JsonNullable<Boolean> isDomestic;
    private JsonNullable<Boolean> isLocked;
    private JsonNullable<Boolean> isNotifyConsigneeEqual;
    private JsonNullable<Boolean> isShipmentReadOnly;
    private JsonNullable<String> jobType;
    private List<JobRequest> jobsList;
    private JsonNullable<String> lockedBy;
    private JsonNullable<String> marksNum;
    private JsonNullable<String> masterBill;
    private JsonNullable<BigDecimal> netWeight;
    private JsonNullable<String> netWeightUnit;
    private JsonNullable<Integer> noOfPacks;
    private List<NotesRequest> notesList;
    private List<PackingRequest> packingList;
    private JsonNullable<String> packsUnit;
    private JsonNullable<String> paymentTerms;
    private List<PickupDeliveryDetailsRequest> pickupDeliveryDetailsList;
    private JsonNullable<Integer> prevShipmentStatus;
    private JsonNullable<Long> receivingBranch;
    private List<ReferenceNumbersRequest> referenceNumbersList;
    private JsonNullable<String> route;
    private List<RoutingsRequest> routingsList;
    private JsonNullable<Long> salesAgent;
    private JsonNullable<String> serviceType;
    private List<ServiceDetailsRequest> servicesList;
    private JsonNullable<String> shipmentCompletedBy;
    private JsonNullable<LocalDateTime> shipmentCompletedOn;
    private JsonNullable<String> shipmentId;
    private JsonNullable<String> shipmentType;
    private JsonNullable<String> source;
    private JsonNullable<Long> sourceTenantId;
    private JsonNullable<Integer> status;
    private JsonNullable<String> transportMode;
    private JsonNullable<Long> triangulationPartner;
    private List<TruckDriverDetailsRequest> truckDriverDetails;
    private JsonNullable<BigDecimal> volume;
    private JsonNullable<String> volumeUnit;
    private JsonNullable<BigDecimal> volumetricWeight;
    private JsonNullable<String> volumetricWeightUnit;
    private JsonNullable<BigDecimal> weight;
    private JsonNullable<String> weightUnit;
}
