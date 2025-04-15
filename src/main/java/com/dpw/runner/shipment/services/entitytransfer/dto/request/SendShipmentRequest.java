package com.dpw.runner.shipment.services.entitytransfer.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.*;

import java.util.List;

@Data
@Builder
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class SendShipmentRequest implements IRunnerRequest {
    private Long shipId;
    private List<String> additionalDocs;
    private List<Integer> sendToBranch;
    private List<String> sendToOrg;
    private Boolean isAutomaticTransfer;
}
