package com.dpw.runner.shipment.services.entitytransfer.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.*;

import java.util.List;
import java.util.Map;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class SendConsolidationRequest implements IRunnerRequest {
    private Long consolId;
    private List<Integer> sendToBranch;
    private Map<String, List<Integer>> shipmentGuidSendToBranch;
    private List<String> sendToOrg;
    private List<String> additionalDocs;
    private Map<String, List<String>> shipAdditionalDocs;
    private Boolean isAutomaticTransfer;
}
