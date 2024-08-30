package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.*;

import java.util.List;
import java.util.UUID;

@Data
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CopyDocumentsRequest extends CommonRequest implements IRunnerRequest {

    private List<DocumentRequest> documents;

    @Data
    @ToString
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    static class DocumentRequest {
        private Integer tenantId;
        private String entityKey;
        private String entityType;
        private List<UUID> docGuid;
    }
}
