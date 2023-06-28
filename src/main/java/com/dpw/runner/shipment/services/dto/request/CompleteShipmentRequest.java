package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.util.List;

@Data
@ApiModel("Complete Shipment Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class CompleteShipmentRequest implements IRunnerRequest {
    private List<AdditionalDetailRequest> additionalDetailRequest;
    private List<ContainerRequest> containerRequest;
}

