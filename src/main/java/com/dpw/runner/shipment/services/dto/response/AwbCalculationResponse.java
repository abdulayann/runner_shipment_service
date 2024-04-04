package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.request.awb.AwbGoodsDescriptionInfo;
import com.dpw.runner.shipment.services.dto.request.awb.AwbOtherChargesInfo;
import com.dpw.runner.shipment.services.dto.request.awb.AwbPackingInfo;
import com.dpw.runner.shipment.services.dto.request.awb.AwbPaymentInfo;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AwbCalculationResponse implements IRunnerRequest {
    private List<AwbGoodsDescriptionInfo> awbGoodsDescriptionInfo;
    private List<AwbOtherChargesInfo> awbOtherChargesInfo;
    private List<AwbPackingInfo> awbPackingInfo;
    private AwbPaymentInfo awbPaymentInfo;
}
