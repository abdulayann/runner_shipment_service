package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ShipmentRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
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
    private Integer assignedTo;
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
    private boolean isLocked;
    private String lockedBy;
    private boolean isNotifyConsigneeEqual;
    private String bookingType;
    private boolean cargoFinanceBooking;
    private String bookingNumber;
    private String route;
    private long SourceTenantId;
    private long documentationPartner;
    private long triangulationPartner;
    private long receivingBranch;
    private boolean intraBranch;
    private Integer prevShipmentStatus;
    private boolean isShipmentReadOnly;
    private String shipmentCompletedBy;
    private LocalDateTime shipmentCompletedOn;
    private String financeClosedBy;
    private LocalDateTime financeClosedOn;
}
