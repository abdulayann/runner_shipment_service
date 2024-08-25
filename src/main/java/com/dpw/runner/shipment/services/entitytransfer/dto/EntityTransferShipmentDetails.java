package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import lombok.*;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class EntityTransferShipmentDetails implements IEntityTranferBaseEntity, Serializable {
    private UUID guid;
    public EntityTransferCarrierDetails carrierDetails;
    public String houseBill;
    public String transportMode;
    public String direction;
    public String shipmentType;
    private List<EntityTransferContainers> containersList;
    public Integer status;
    public String source;
    public String jobType;
    public String serviceType;
    public String masterBill;
    public String bookingReference;
    public String consolRef;
    public Long salesAgent;
    public String paymentTerms;
    public String incoterms;
    public String shipmentId;
    public Boolean isDomestic;
    public String assignedTo;
    public String additionalTerms;
    public String goodsDescription;
    private List<EntityTransferBookingCarriage> bookingCarriagesList;
    private List<EntityTransferFileRepo> fileRepoList;
    private List<EntityTransferPacking> packingList;
    private List<EntityTransferReferenceNumbers> referenceNumbersList;
    private List<EntityTransferServiceDetails> servicesList;
    public BigDecimal weight;
    public String weightUnit;
    public BigDecimal volume;
    public String volumeUnit;
    public BigDecimal volumetricWeight;
    public String volumetricWeightUnit;
    public BigDecimal chargable;
    public String chargeableUnit;
    public BigDecimal netWeight;
    public String netWeightUnit;
    public Integer noOfPacks;
    public String packsUnit;
    public Integer innerPacks;
    public String innerPackUnit;
    public Integer freightLocal;
    public String freightLocalCurrency;
    public Integer freightOverseas;
    public String freightOverseasCurrency;
    public boolean autoUpdateWtVol;
    public boolean containerAutoWeightVolumeUpdate;
    public String marksNum;
    public String entryDetail;
    public Boolean isNotifyConsigneeEqual;
    public String bookingType;
    public boolean cargoFinanceBooking;
    public String bookingNumber;
    public String route;
    public long sourceTenantId;
    public long documentationPartner;
    public long triangulationPartner;
    public long receivingBranch;
    public boolean intraBranch;
    public Integer prevShipmentStatus;
    public String shipmentCompletedBy;
    public LocalDateTime shipmentCompletedOn;
    public String financeClosedBy;
    public LocalDateTime financeClosedOn;
    public EntityTransferAdditionalDetails additionalDetails;
    public EntityTransferParties client;
    public EntityTransferParties consigner;
    public EntityTransferParties consignee;

    private String sourceBranchTenantName;

    private Map<String, EntityTransferMasterLists> masterData;
    private Map<String, EntityTransferUnLocations> unlocationData;
    private Map<String, EntityTransferUser> userMasterData;
    private Map<String, EntityTransferCurrency> currenciesMasterData;

}