package com.dpw.runner.shipment.services.syncing.Entity;

import lombok.Data;

import java.util.List;
import java.util.UUID;

@Data
public class AwbRequestV2 {
    public UUID guid;
    public UUID shipmentGuid;
    public UUID consolidationGuid;
    public AwbShipmentInfoV2 awbShipmentInfo;
    public List<AwbNotifyPartyInfoV2> awbNotifyPartyInfo;
    public List<AwbRoutingInfoV2> awbRoutingInfo;
    public AwbCargoInfoV2 awbCargoInfo;
    public AwbPaymentInfoV2 awbPaymentInfo;
    public List<AwbOtherChargesInfoV2> awbOtherChargesInfo;
    public AwbOtherInfoV2 awbOtherInfo;
    public List<AwbOCIInfoV2> awbOciInfo;
    public List<AwbGoodsDescriptionInfoV2> awbGoodsDescriptionInfo;
    public List<AwbPackingInfoV2> awbPackingInfo;
    public List<AwbSpecialHandlingCodesMappingInfoV2> awbSpecialHandlingCodesMappings;
}
