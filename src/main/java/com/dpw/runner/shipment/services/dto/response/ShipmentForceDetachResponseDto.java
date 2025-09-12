package com.dpw.runner.shipment.services.dto.response;


import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.util.List;

@Data
@Builder
@ApiModel("Shipment Force Detach Response")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class ShipmentForceDetachResponseDto implements IRunnerResponse {
    private List<ShipmentForceDetachResponse> shipmentDetachResponses;
    private Boolean isContainerOrPackageAttached;
}
