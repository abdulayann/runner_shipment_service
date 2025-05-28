package com.dpw.runner.shipment.services.entitytransfer.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.*;

import java.util.List;
import java.util.Map;

@Data
@Builder
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class SendFileToExternalRequest implements IRunnerRequest {
    private Long entityId;
    private String entityType;
    private String sendToBranch;
    private List<String> additionalDocs;
    private Map<String, List<String>> shipAdditionalDocs;
}
