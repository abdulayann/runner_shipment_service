package com.dpw.runner.shipment.services.dto.request.awb;

import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.MasterData;
import com.dpw.runner.shipment.services.utils.UnlocationData;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import io.swagger.annotations.ApiModel;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;
import lombok.*;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@Data
@Builder
@ApiModel("AWB Goods Description Info Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class AwbGoodsDescriptionInfo implements Serializable {
    private Long entityId;
    private String entityType;
    private Integer piecesNo;
    private BigDecimal grossWt;
    @MasterData(type = MasterDataType.WEIGHT_UNIT)
    private String grossWtUnit;
    private BigDecimal grossVolume;
    @MasterData(type = MasterDataType.VOLUME_UNIT)
    private String grossVolumeUnit;
    private Integer rateClass;
    private Integer commodityItemNo;
    private BigDecimal chargeableWt;
    private BigDecimal rateCharge;
    private BigDecimal totalAmount;
    private Integer slaCCode;
    private String hsCode;
    private UUID guid;
    private List<AwbPackingInfo> awbPackingInfo;
    private Boolean isShipmentCreated;
    private Boolean disableFetchRates;
    private Boolean disableRates;
    private Boolean enableFetchRatesWarning;
    private String ntrQtyGoods;
    private String dimensions;
    private String rcp;

    @Size(max = 5, message = "ULD Serial Number must be up to 5 alphanumeric characters.")
    @Pattern(regexp = "^[A-Za-z0-9]*$", message = "ULD Serial Number must be alphanumeric.")
    private String uldSerialNumber;

    private BigDecimal uldTareWeight;
    private String uldTareWeightUnit;

    @Size(max = 3, message = "ULD Type must be up to 3 alphanumeric characters.")
    @Pattern(regexp = "^[A-Za-z0-9]*$", message = "ULD Type must be alphanumeric.")
    private String uldType;

    @Size(max = 2, message = "ULD Code must be up to 2 alphanumeric characters.")
    @Pattern(regexp = "^[A-Za-z0-9]*$", message = "ULD Code must be alphanumeric.")
    private String uldCode;

    @Size(max = 3, message = "ULD Rate Type Code must be up to 3 alphanumeric characters.")
    @Pattern(regexp = "^[A-Za-z0-9]*$", message = "ULD Rate Type Code must be alphanumeric.")
    private String uldRateTypeCode;

    @Pattern(regexp = "^[A-Za-z]$", message = "Basis Code must be a single alphabetic character.")
    private String basisCode;

    @Pattern(
        regexp = "^\\d{1,3}(\\.\\d+)?$",
        message = "Applied Percent must be numeric with up to 3 digits, including decimals."
    )
    private String appliedPercent;

    @Size(max = 35, message = "Reference ID must be up to 35 alphanumeric characters.")
    @Pattern(regexp = "^[A-Za-z0-9]*$", message = "Reference ID must be alphanumeric.")
    private String referenceId;

    @Size(max = 35, message = "Reference Type Code must be up to 35 alphanumeric characters.")
    @Pattern(regexp = "^[A-Za-z0-9]*$", message = "Reference Type Code must be alphanumeric.")
    private String referenceTypeCode;
}
