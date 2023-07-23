package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.Getter;
import lombok.ToString;

@Getter
@ApiModel("Consolidation Shipment Linking Request Model")
@ToString
public class ConsolShipLinkRequest extends CommonRequest implements IRunnerRequest {
    private Long consolidationId;
    private Long shipmentId;
}
