package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.*;

import java.io.Serializable;
import java.util.List;

@Data
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CopyDocumentsRequest extends CommonRequest implements IRunnerRequest {

    private List<DocumentRequest> documents;
    private Boolean deleteExistingDocuments;

    @Data
    @ToString
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class DocumentRequest implements Serializable {
        private Integer tenantId;
        private String entityKey;
        private String entityType;
        private List<String> docGuid;
    }
}
