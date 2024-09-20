package com.dpw.runner.shipment.services.entitytransfer.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.*;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
public class CheckTaskExistRequest implements IRunnerRequest {
    private List<Integer> sendToBranch;
    private List<String> sendToOrg;
    private Long entityId;
    private String entityType;
}
