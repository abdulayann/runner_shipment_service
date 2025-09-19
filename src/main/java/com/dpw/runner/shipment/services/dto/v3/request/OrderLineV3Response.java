package com.dpw.runner.shipment.services.dto.v3.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import lombok.*;

import javax.validation.constraints.Size;
import java.math.BigDecimal;
import java.util.UUID;

@Getter
@Setter
@ApiModel(value = "Order line V3 request model")
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class OrderLineV3Response extends CommonRequest implements IRunnerRequest {
    private Long id;
    private UUID guid;

    private String orderNo;

    @JsonProperty("commodityCode")
    private String commodityGroup;

    @JsonProperty("container")
    private Long containerId;

    @Size(max = 25000, message = "Max size is 25000 for goods description")
    @JsonProperty("itemDescription")
    private String goodsDescription;

    @JsonProperty("hsCode")
    private String HSCode;

    private BigDecimal length;
    @JsonProperty("lengthUOM")
    private String lengthUnit;

    private BigDecimal width;
    @JsonProperty("widthUOM")
    private String widthUnit;

    @JsonProperty("height")
    private BigDecimal height;
    @JsonProperty("heightUOM")
    private String heightUnit;

    private BigDecimal weight;
    @JsonProperty("weightUOM")
    private String weightUnit;

    private BigDecimal volume;
    @JsonProperty("volumeUOM")
    private String volumeUnit;

    private BigDecimal netWeight;
    @JsonProperty("netWeightUOM")
    private String netWeightUnit;

    @JsonProperty("quantityReceived")
    private String packs;

    @JsonProperty("quantityUnit")
    private String packsType;

    private BigDecimal lineNo;

    private BigDecimal subLineNo;

    private String productCode;

    private Long shipmentOrderId;
}
