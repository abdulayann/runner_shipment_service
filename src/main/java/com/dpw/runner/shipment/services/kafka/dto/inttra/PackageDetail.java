package com.dpw.runner.shipment.services.kafka.dto.inttra;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.util.List;

@Getter
@Setter
public class PackageDetail implements Serializable {
    private String packageType;
    private Integer lineNumber;
    private Integer count;
    private String typeCode;
    private String typeValue;
    private String typeDescription;
    private String goodsDescription;
    private String goodsClassificationType;
    private String goodsClassificationValue;
    private String goodsClassificationSchedBValue;
    private Weight goodsGrossWeight;
    private Volume goodsGrossVolume;
    private List<String> goodsReferences;
    private List<String> marks;
    private String outOfGaugeDetails;
    private List<String> splitGoodsDetailsList;
    private String dangerousGoods;
}
