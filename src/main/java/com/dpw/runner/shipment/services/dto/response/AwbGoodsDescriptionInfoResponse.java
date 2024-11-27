package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.awb.AwbPackingInfo;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.MasterData;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Data
public class AwbGoodsDescriptionInfoResponse implements IRunnerResponse {
    private Long entityId;
    private String entityType;
    private Integer piecesNo;
    private BigDecimal grossWt;
    private String grossWtUnit;
    private BigDecimal grossVolume;
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
    private String rcp;
    private String uldSerialNumber;
    private BigDecimal uldTareWeight;
    private String uldTareWeightUnit;
    private String uldType;
    private String uldCode;
    private String uldRateTypeCode;
    private String basisCode;
    private String appliedPercent;
    private String referenceId;
    private String referenceTypeCode;

    //Master Data
    private Map<String, String> masterData;
    private Map<String, String> unlocationData;
    private String ntrQtyGoods;
    private String dimensions;
}
