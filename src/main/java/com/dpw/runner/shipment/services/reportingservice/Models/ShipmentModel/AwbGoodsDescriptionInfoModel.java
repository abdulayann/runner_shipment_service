package com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel;

import com.dpw.runner.shipment.services.reportingservice.Models.IDocumentModel;
import com.dpw.runner.shipment.services.dto.request.awb.AwbPackingInfo;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class AwbGoodsDescriptionInfoModel implements IDocumentModel {
    @JsonProperty("EntityId")
    private Long entityId;
    @JsonProperty("EntityType")
    private String entityType;
    @JsonProperty("PiecesNo")
    private Integer piecesNo;
    @JsonProperty("GrossWt")
    private BigDecimal grossWt;
    @JsonProperty("GrossWtUnit")
    private String grossWtUnit;
    @JsonProperty("GrossVolume")
    private BigDecimal grossVolume;
    @JsonProperty("GrossVolumeUnit")
    private String grossVolumeUnit;
    @JsonProperty("RateClass")
    private Integer rateClass;
    @JsonProperty("CommodityItemNo")
    private Integer commodityItemNo;
    @JsonProperty("ChargeableWt")
    private BigDecimal chargeableWt;
    @JsonProperty("RateCharge")
    private BigDecimal rateCharge;
    @JsonProperty("TotalAmount")
    private BigDecimal totalAmount;
    @JsonProperty("SlaCCode")
    private Integer slaCCode;
    @JsonProperty("HsCode")
    private String hsCode;
    @JsonProperty("Rcp")
    private String rcp;
    private UUID guid;
    private List<AwbPackingInfo> awbPackingInfo;
    private Boolean isShipmentCreated;
    @JsonProperty("NtrQtyGoods")
    private String ntrQtyGoods;
    @JsonProperty("Dimensions")
    private String dimensions;
}
