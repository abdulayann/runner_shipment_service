package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Data;

@Data
public class DetachPacksListDto implements IRunnerRequest {
    private Long shipmentId;
    private Long containerId;
    private Integer pageNo;
    private Integer pageSize;
}
