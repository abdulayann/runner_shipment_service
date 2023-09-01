package com.dpw.runner.shipment.services.entitytransfer.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferShipmentDetails;
import lombok.*;

import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class SendShipmentResponse implements IRunnerResponse {
    List<Integer> successTenantIds;
    EntityTransferShipmentDetails entityTransferShipmentDetails;
}
