package com.dpw.runner.shipment.services.document.request.documentmanager;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class DocumentManagerSaveFileRequest {
    private String entityType;

    private Long entityId;

    private String fileName;

    private String path;

    private String secureDownloadLink;

    private Long fileSize;

    private String fileType;

    private String docType;

    private String eventCode;

    private Boolean customerPortalVisibility;
    private String source;
    private String docName;
    private String entityKey;
}
