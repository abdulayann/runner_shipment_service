package com.dpw.runner.shipment.services.service.TO.request;

import com.dpw.runner.shipment.services.dto.request.awb.*;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.ArrayList;

@Getter
@Setter
@JsonIgnoreProperties(ignoreUnknown = true)
@ToString
public class AwbData {
    public Integer id;
    public String guid;
    public String awbNumber;
    public String shipmentId;
    public AwbShipmentInfo awbShipmentInfo;
    public ArrayList<AwbNotifyPartyInfo> awbNotifyPartyInfo;
    public ArrayList<AwbRoutingInfo> awbRoutingInfo;
    public AwbCargoInfo awbCargoInfo;
    public AwbPaymentInfo awbPaymentInfo;
    public ArrayList<AwbOtherChargesInfo> awbOtherChargesInfo;
    public AwbOtherInfo awbOtherInfo;
    public ArrayList<AwbOciInfo> awbOciInfo;
    public ArrayList<AwbGoodsDescriptionInfo> awbGoodsDescriptionInfo;
    public ArrayList<AwbPackingInfo> awbPackingInfo;
    public ArrayList<AwbSpecialHandlingCodesMappingInfo> awbSpecialHandlingCodesMappings;
    public AwbKafkaEntity awbKafkaEntity;
    public MetaData meta;
}
