package com.dpw.runner.shipment.services.dto.request.awb;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import lombok.Data;

import java.util.List;

@Data
public class GenerateAwbPaymentInfoRequest implements IRunnerRequest {
    private Boolean isFromShipment;
    private List<AwbGoodsDescriptionInfo> awbGoodsDescriptionInfo;
    private List<AwbOtherChargesInfo> awbOtherChargesInfo;
    private List<AwbPackingInfo> awbPackingInfo;
    private AwbCargoInfo awbCargoInfo;
    private AwbPaymentInfo awbPaymentInfo;
    private MasterData chargeDetails;
}
