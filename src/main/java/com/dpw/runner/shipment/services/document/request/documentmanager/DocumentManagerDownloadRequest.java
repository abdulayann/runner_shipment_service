package com.dpw.runner.shipment.services.document.request.documentmanager;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class DocumentManagerDownloadRequest {
    private String entityKey;
    private String entityType;
    private String docType;
    private String childType;
}
