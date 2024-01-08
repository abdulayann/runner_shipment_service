package com.dpw.runner.shipment.services.dto.request.awb;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Data;

import java.util.List;

@Data
public class GenerateAwbPaymentInfoRequest implements IRunnerRequest {
    private Boolean isFromShipment;
    private List<AwbGoodsDescriptionInfo> awbGoodsDescriptionInfo;
    private List<AwbOtherChargesInfo> awbOtherChargesInfo;
    private List<AwbPackingInfo> awbPackingInfo;
}
