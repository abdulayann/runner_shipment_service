package com.dpw.runner.shipment.services.service_bus.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Data
public class ContainerBoomiUniversalJson {
    @JsonProperty("BookingId")
    private Long bookingId;
    @JsonProperty("ShippingInstructionId")
    private Long shippingInstructionId;
    @JsonProperty("ConsolidationId")
    private Long consolidationId;
    @JsonProperty("ShipmentId")
    private Long shipmentId;
    @JsonProperty("QuoteId")
    private Long quoteId;
    @JsonProperty("Guid")
    private UUID guid;
    @JsonProperty("EntityType")
    private String entityType;
    @JsonProperty("EntityId")
    private Long entityId;
    @JsonProperty("ContainerTypeId")
    private Long containerTypeId;
    @JsonProperty("ContainerNumber")
    private String containerNumber;
    @JsonProperty("SealNumber")
    private String sealNumber;
    @JsonProperty("DescriptionOfGoods")
    private String descriptionOfGoods;
    @JsonProperty("NoofPackages")
    private Integer noofPackages;
    @JsonProperty("NetWeight")
    private BigDecimal netWeight;
    @JsonProperty("NetWeightUnit")
    private String netWeightUnit;
    @JsonProperty("GrossWeight")
    private BigDecimal grossWeight;
    @JsonProperty("GrossWeightUnit")
    private String grossWeightUnit;
    @JsonProperty("GrossVolume")
    private BigDecimal grossVolume;
    @JsonProperty("GrossVolumeUnit")
    private String grossVolumeUnit;
    @JsonProperty("Measurement")
    private BigDecimal measurement;
    @JsonProperty("MeasurementUnit")
    private String measurementUnit;
    @JsonProperty("CommodityId")
    private Long commodityId;
    @JsonProperty("HsCode")
    private String hsCode;
    @JsonProperty("IsShipperOwned")
    private Boolean isShipperOwned;
    @JsonProperty("Hazardous")
    private Boolean hazardous;
    @JsonProperty("HazardousGoodType")
    private String hazardousGoodType;
    @JsonProperty("HazardousUn")
    private String hazardousUn;
    @JsonProperty("CargoType")
    private String cargoType;
    @JsonProperty("IsEmpty")
    private Boolean isEmpty;
    @JsonProperty("ContainerCount")
    private String containerCount;
    @JsonProperty("CarrierSealNumber")
    private String carrierSealNumber;
    @JsonProperty("ShipperSealNumber")
    private String shipperSealNumber;
    @JsonProperty("TerminalOperatorSealNumber")
    private String terminalOperatorSealNumber;
    @JsonProperty("VeterinarySealNumber")
    private String veterinarySealNumber;
    @JsonProperty("CustomsSealNumber")
    private String customsSealNumber;
    @JsonProperty("CustomsReleaseCode")
    private String customsReleaseCode;
    @JsonProperty("ContainerStuffingLocation")
    private Long containerStuffingLocation;
    @JsonProperty("ContainerComments")
    private String containerComments;
    @JsonProperty("IsReefer")
    private Boolean isReefer;
    @JsonProperty("MinTemp")
    private BigDecimal minTemp;
    @JsonProperty("MaxTemp")
    private BigDecimal maxTemp;
    @JsonProperty("MinTempUnit")
    private String minTempUnit;
    @JsonProperty("MaxTempUnit")
    private String maxTempUnit;
    @JsonProperty("Mode")
    private String mode;
    @JsonProperty("TenantId")
    private Integer tenantId;
    @JsonProperty("PerContainerCostRate")
    private String perContainerCostRate;
    @JsonProperty("PerContainerSellRate")
    private String perContainerSellRate;
    @JsonProperty("CurrentCostRate")
    private BigDecimal currentCostRate;
    @JsonProperty("MinimumCost")
    private BigDecimal minimumCost;
    @JsonProperty("TotalCostValue")
    private BigDecimal totalCostValue;
    @JsonProperty("CurrentSellRate")
    private BigDecimal currentSellRate;
    @JsonProperty("MinimumSell")
    private BigDecimal minimumSell;
    @JsonProperty("TotalSellValue")
    private BigDecimal totalSellValue;
    @JsonProperty("PickupAddress")
    private Long pickupAddress;
    @JsonProperty("DeliveryAddress")
    private Long deliveryAddress;
    @JsonProperty("TransportInstructionId")
    private Long transportInstructionId;
    @JsonProperty("AllocationDate")
    private LocalDateTime allocationDate;
    @JsonProperty("IsActive")
    private Integer isActive;
    @JsonProperty("InsertUserId")
    private Long insertUserId;
    @JsonProperty("InsertDate")
    private LocalDateTime insertDate;
    @JsonProperty("UpdateUserId")
    private Long updateUserId;
    @JsonProperty("UpdateDate")
    private LocalDateTime updateDate;
}
