package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entity.enums.ContainerStatus;
import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Map;
import java.util.UUID;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class EntityTransferContainers implements IEntityTranferBaseEntity {
    public UUID guid;
    public String containerCode;
    public String containerNumber;
    public String sealNumber;
    public String descriptionOfGoods;
    public Long noOfPackages;
    public BigDecimal netWeight;
    public String netWeightUnit;
    public BigDecimal grossWeight;
    public String grossWeightUnit;
    public BigDecimal measurement;
    public String measurementUnit;
    public String commodityCode;
    public String hsCode;
    public Boolean isShipperOwned;
    public Boolean isEmpty;
    public Long containerCount;
    public String carrierSealNumber;
    public String shipperSealNumber;
    public String terminalOperatorSealNumber;
    public String veterinarySealNumber;
    public String customsSealNumber;
    public String customsReleaseCode;
    public String containerStuffingLocation;
    public String containerComments;
    public BigDecimal grossVolume;
    public String grossVolumeUnit;
    public Boolean isReefer;
    public BigDecimal minTemp;
    public String minTempUnit;
    public BigDecimal maxTemp;
    public String maxTempUnit;
    public String hblDeliveryMode;
    public LocalDateTime allocationDate;
    public String dgClass;
    public Boolean hazardous;
    public String hazardousUn;
    public BigDecimal tareWeight;
    public String tareWeightUnit;
    public String serialNumber;
    public String innerPackageNumber;
    public String innerPackageType;
    public BigDecimal packageLength;
    public BigDecimal packageBreadth;
    public BigDecimal packageHeight;
    public Boolean isTemperatureMaintained;
    public String packs;
    public String packsType;
    public String marksNums;
    public String innerPackageMeasurementUnit;
    public String pacrNumber;
    public BigDecimal chargeable;
    public String chargeableUnit;
    public Boolean isOwnContainer;
    public String transportMode;
    public ContainerStatus status;
    public String extraParams;
    public String remarks;
    public BigDecimal allocatedWeight;
    public String allocatedWeightUnit;
    public BigDecimal allocatedVolume;
    public String allocatedVolumeUnit;
    public BigDecimal achievedWeight;
    public String achievedWeightUnit;
    public BigDecimal achievedVolume;
    public String achievedVolumeUnit;
    public String weightUtilization;
    public String volumeUtilization;
    private Map<String, EntityTransferMasterLists> masterData;
    private Map<String, EntityTransferContainerType> containerTypeMasterData;
    private Map<String, EntityTransferCommodityType> commodityTypeMasterData;
}