package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomVolumeValueSerializer;
import com.dpw.runner.shipment.services.config.CustomWeightValueSerializer;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import io.swagger.annotations.ApiModel;
import java.util.Map;
import lombok.Data;
import lombok.Setter;

import java.math.BigDecimal;
import java.util.UUID;

@Data
@Setter
@ApiModel("Container Response Model")
public class ContainerBaseResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private String containerCode;
    private String containerNumber;
    private String sealNumber;
    private String descriptionOfGoods;
    @JsonSerialize(using = CustomWeightValueSerializer.class)
    private BigDecimal netWeight;
    private String netWeightUnit;
    @JsonSerialize(using = CustomWeightValueSerializer.class)
    private BigDecimal grossWeight;
    private Map<String, String> unlocationData;
    private Map<String, String> masterData;
    private Map<String, String> commodityTypeData;
    private String grossWeightUnit;
    private String hsCode;
    private String packs;
    private String packsType;
    private String marksNums;
    private BigDecimal tareWeight;
    private String tareWeightUnit;
    @JsonSerialize(using = CustomVolumeValueSerializer.class)
    private BigDecimal grossVolume;
    private String grossVolumeUnit;
    private String dgClass;
    private Boolean hazardous;
    private String unNumber;
    private String properShippingName;
    private String packingGroup;
    private BigDecimal minimumFlashPoint;
    private String minimumFlashPointUnit;
    private Boolean isReefer;
    private Boolean isShipperOwned;
    private Boolean isEmpty;
    private Boolean isOwnContainer;
    private String customsReleaseCode;
    private String containerStuffingLocation;
    private String invoiceNumber;
    private String invoiceCurrency;
    private BigDecimal invoiceValue;
    private String handlingInfo;
    private BigDecimal minTemp;
    private String minTempUnit;
    private BigDecimal maxTemp;
    private String maxTempUnit;
    private Boolean marinePollutant;
    private String assignedContainer;
}
