package com.dpw.runner.shipment.services.document.request.documentmanager;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class DocumentManagerEntityFileRequest {
    private String entityKey;
    private String entityType;
    private Long tenantId;
}
