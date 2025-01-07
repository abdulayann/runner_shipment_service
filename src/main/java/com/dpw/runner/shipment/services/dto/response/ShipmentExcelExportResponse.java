package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.entity.enums.FileStatus;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Set;
import java.util.UUID;


@Builder
@NoArgsConstructor
@AllArgsConstructor
@Data
public class ShipmentExcelExportResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Integer tenantId;
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
    private BigDecimal freightLocal;
    private String freightLocalCurrency;
    private BigDecimal freightOverseas;
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
    private long sourceTenantId;
    private long documentationPartner;
    private List<Long> triangulationPartnerList;
    private Long triangulationPartner;
    private long receivingBranch;
    private boolean intraBranch;
    private Integer prevShipmentStatus;
    private boolean isShipmentReadOnly;
    private String shipmentCompletedBy;
    private LocalDateTime shipmentCompletedOn;
    private String financeClosedBy;
    private LocalDateTime financeClosedOn;
    private PartiesResponse client;
    private PartiesResponse consigner;
    private PartiesResponse consignee;
    private AdditionalDetailsListResponse additionalDetails;
    private CarrierDetailResponse carrierDetails;
    private Long container20Count;
    private Long container40Count;
    private Long container20GPCount;
    private Long container20RECount;
    private Long container40GPCount;
    private Long container40RECount;
    private Set<String> containerNumbers;
    private PickupDeliveryDetailsListResponse pickupDetails;
    private PickupDeliveryDetailsListResponse deliveryDetails;
    private String createdBy;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    private LocalDateTime invoiceDate;
    private LocalDateTime taxDate;
    private LocalDateTime customsFilingDate;
    private LocalDateTime amsFilingDate;
    private BigDecimal teuCount;
    private String shipmentStatus;
    private String billStatus;
    private BigDecimal totalEstimatedCost;
    private BigDecimal totalEstimatedRevenue;
    private BigDecimal totalEstimatedProfit;
    private BigDecimal totalEstimatedProfitPercent;
    private BigDecimal totalCost;
    private BigDecimal totalRevenue;
    private BigDecimal totalProfit;
    private BigDecimal totalProfitPercent;
    private BigDecimal totalPostedCost;
    private BigDecimal totalPostedRevenue;
    private BigDecimal totalPostedProfit;
    private BigDecimal totalPostedProfitPercent;
    private String wayBillNumber;
    private String orderManagementNumber;
    private String orderManagementId;
    private String jobStatus;
    private Boolean containsHazardous;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime requestedOn;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime shipmentCreatedOn;
    private FileStatus fileStatus;
    private Boolean isReceivingBranchAdded;
    private String department;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime cargoDeliveryDate;
    private Boolean isNetworkFile;
    private Boolean isReceivingBranchManually;
}
