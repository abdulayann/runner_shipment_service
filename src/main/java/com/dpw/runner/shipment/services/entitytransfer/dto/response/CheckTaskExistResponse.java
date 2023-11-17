package com.dpw.runner.shipment.services.entitytransfer.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.*;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
public class CheckTaskExistResponse implements IRunnerResponse {
    private List<Integer> sendToBranch;
    private List<String> sendToOrg;
}
