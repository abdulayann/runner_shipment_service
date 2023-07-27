package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ShipmentRequest extends CommonRequest implements IRunnerRequest {
    private AdditionalDetailRequest additionalDetail;
    private String additionalTerms;
    private Integer assignedTo;
    private Boolean autoUpdateWtVol;
    private List<BookingCarriageRequest> bookingCarriagesList;
    private String bookingNumber;
    private String bookingReference;
    private String bookingType;
    private Boolean cargoFinanceBooking;
    private CarrierDetailRequest carrierDetails;
    private BigDecimal chargable;
    private String chargeableUnit;
    private PartiesRequest client;
    private PartiesRequest consignee;
    private PartiesRequest consigner;
    private String consolRef;
    private Boolean containerAutoWeightVolumeUpdate;
    private List<ContainerRequest> containersList;
    private String direction;
    private Long documentationPartner;
    private List<ELDetailsRequest> elDetailsList;
    private String entryDetail;
    private List<EventsRequest> eventsList;
    private List<FileRepoRequest> fileRepoList;
    private String financeClosedBy;
    private LocalDateTime financeClosedOn;
    private Integer freightLocal;
    private String freightLocalCurrency;
    private Integer freightOverseas;
    private String freightOverseasCurrency;
    private String goodsDescription;
    private String houseBill;
    private Long id;
    private String incoterms;
    private String innerPackUnit;
    private Integer innerPacks;
    private Boolean intraBranch;
    private Boolean isDomestic;
    private Boolean isLocked;
    private Boolean isNotifyConsigneeEqual;
    private Boolean isShipmentReadOnly;
    private String jobType;
    private List<JobRequest> jobsList;
    private String lockedBy;
    private String marksNum;
    private String masterBill;
    private BigDecimal netWeight;
    private String netWeightUnit;
    private Integer noOfPacks;
    private List<NotesRequest> notesList;
    private List<PackingRequest> packingList;
    private String packsUnit;
    private String paymentTerms;
    private List<PickupDeliveryDetailsRequest> pickupDeliveryDetailsList;
    private Integer prevShipmentStatus;
    private Long receivingBranch;
    private List<ReferenceNumbersRequest> referenceNumbersList;
    private String route;
    private List<RoutingsRequest> routingsList;
    private Long salesAgent;
    private String serviceType;
    private List<ServiceDetailsRequest> servicesList;
    private String shipmentCompletedBy;
    private LocalDateTime shipmentCompletedOn;
    private String shipmentId;
    private String shipmentType;
    private String source;
    private Long sourceTenantId;
    private Integer status;
    private String transportMode;
    private Long triangulationPartner;
    private List<TruckDriverDetailsRequest> truckDriverDetails;
    private BigDecimal volume;
    private String volumeUnit;
    private BigDecimal volumetricWeight;
    private String volumetricWeightUnit;
    private BigDecimal weight;
    private String weightUnit;
    private List<ConsolidationDetailsRequest> consolidationList;
    private String jobStatus;
}
