package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.awb.*;
import lombok.Data;

import java.util.List;
import java.util.UUID;

@Data
public class AwbResponse implements IRunnerResponse {
    private Integer id;
    private UUID guid;
    private AwbShipmentInfo awbShipmentInfo;
    public List<AwbNotifyPartyInfo> awbNotifyPartyInfo;
    public List<AwbRoutingInfo> awbRoutingInfo;
    public AwbCargoInfo awbCargoInfo;
    public AwbPaymentInfo awbPaymentInfo;
    public List<AwbOtherChargesInfo> awbOtherChargesInfo;
    public AwbOtherInfo awbOtherInfo;
    public List<AwbOCIInfo> awbOciInfo;
    public List<AwbGoodsDescriptionInfo> awbGoodsDescriptionInfo;
    public List<AwbPackingInfo> awbPackingInfo;
    public List<AwbSpecialHandlingCodesMappingInfo> awbSpecialHandlingCodesMappings;
}
