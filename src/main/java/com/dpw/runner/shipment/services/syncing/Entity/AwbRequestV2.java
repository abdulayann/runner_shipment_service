package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Data;

import java.util.List;
import java.util.UUID;

@Data
public class AwbRequestV2 implements IRunnerRequest {
    public UUID guid;
    public UUID shipmentGuid;
    public UUID consolidationGuid;
    public AwbShipmentInfoV2 awbShipmentInfo;
    private List<AwbNotifyPartyInfoV2> awbNotifyPartyInfo;
    private List<AwbRoutingInfoV2> awbRoutingInfo;
    public AwbCargoInfoV2 awbCargoInfo;
    public AwbPaymentInfoV2 awbPaymentInfo;
    private List<AwbOtherChargesInfoV2> awbOtherChargesInfo;
    public AwbOtherInfoV2 awbOtherInfo;
  //  private List<AwbOCIInfoV2> awbOciInfo;
    private List<AwbGoodsDescriptionInfoV2> awbGoodsDescriptionInfo;
    private List<AwbPackingInfoV2> awbPackingInfo;
    private List<AwbSpecialHandlingCodesMappingInfoV2> awbSpecialHandlingCodesMappings;
    public SaveStatus saveStatus;
}
