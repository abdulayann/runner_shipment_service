package com.dpw.runner.shipment.services.document.response;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;

@Data
@JsonInclude(JsonInclude.Include.NON_NULL)
public class DocumentManagerEntityFileResponse {
    private String fileId;

    private String guid;

    private String source;

    private String fileName;

    private Long fileSize;

    private String fileType;

    private String path;

    private String entityType;

    private String entityId;

    private String childType;

    private String childId;

    private String secureDownloadLink;

    private String createdAt;

    private String postingStatus;

    private Boolean isTransferEnabled;

    private String entityDetails;
}
