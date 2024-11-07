package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.*;

@Data
@Builder
@ApiModel("Transferred Network Transfer Request Model")
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class TransferredNetworkTransferRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
}
