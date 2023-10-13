package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Builder
@NoArgsConstructor
@AllArgsConstructor
@Data
public class ShipmentDetailsResponse implements IRunnerResponse {
    private Long id;
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
    private Integer freightLocal;
    private String freightLocalCurrency;
    private Integer freightOverseas;
    private String freightOverseasCurrency;
    private boolean autoUpdateWtVol;
    private boolean containerAutoWeightVolumeUpdate;
    private String marksNum;
    private String entryDetail;
    private Boolean isLocked;
    private String lockedBy;
    private Boolean isNotifyConsigneeEqual;
    private String bookingType;
    private boolean cargoFinanceBooking;
    private String bookingNumber;
    private String route;
    private Long SourceTenantId;
    private Long documentationPartner;
    private Long triangulationPartner;
    private Long receivingBranch;
    private boolean intraBranch;
    private Integer prevShipmentStatus;
    private boolean isShipmentReadOnly;
    private String shipmentCompletedBy;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime shipmentCompletedOn;
    private String financeClosedBy;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime financeClosedOn;
    private PartiesResponse client;
    private PartiesResponse consigner;
    private PartiesResponse consignee;
    private AdditionalDetailResponse additionalDetails;
    private CarrierDetailResponse carrierDetails;
    private PickupDeliveryDetailsResponse pickupDetails;
    private PickupDeliveryDetailsResponse deliveryDetails;
    private List<NotesResponse> notesList;
    private List<TruckDriverDetailsResponse> truckDriverDetails;
    private List<ServiceDetailsResponse> servicesList;
    private List<RoutingsResponse> routingsList;
    private List<ReferenceNumbersResponse> referenceNumbersList;
    private List<PackingResponse> packingList;
    private List<FileRepoResponse> fileRepoList;
    private List<EventsResponse> eventsList;
    private List<ELDetailsResponse> elDetailsList;
    private List<BookingCarriageResponse> bookingCarriagesList;
    private List<JobResponse> jobsList;
    private List<ContainerResponse> containersList;
    private Long container20Count;
    private Long container40Count;
    private Long container20GPCount;
    private Long container20RECount;
    private Long container40GPCount;
    private Long container40RECount;
    private String jobStatus;
    public Map<String, String> masterData;
    public Map<String, String> unlocationData;
    public Map<String, String> currenciesMasterData;
    public BigDecimal goodsValue;
    public String goodsValueCurrency;
    public BigDecimal insuranceValue;
    public String InsuranceValueCurrency;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime shipmentCreatedOn;
    private String entryRefNo;
    private List<PartiesResponse> shipmentAddresses;
}
