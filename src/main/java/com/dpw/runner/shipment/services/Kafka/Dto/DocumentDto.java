package com.dpw.runner.shipment.services.Kafka.Dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data @Builder
@AllArgsConstructor @NoArgsConstructor
public class DocumentDto {
    private Document data;
    private String action; // CREATE/UPDATE/DELETE

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class Document {
        private String fileId;
        private String guid;
        private String docType;
        private String entityId;
        private String entityType;
        private Boolean customerPortalVisibility;
        private String source;
        private String documentMasterId;
        private String docName;
        private String path;
        private String fileName;
        private String secureDownloadLink;
        private String fileType;
        private String uploadedBy;
    }
}

