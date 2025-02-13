package com.dpw.runner.shipment.services.kafka.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
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
        private String eventCode;
        private String childId;
        private String childType;
        private String validFrom;
        private String validTo;
        private String dateReceived;
        private String createdAt;
        private Object updatedAt; // getting this field as int[] in case of update actions
        private String updatedBy;
        private String physicalDocWith;
        private String fileSource;
        private Long fileSize;
        private Long createdBy;
        private Long tenantId;
        private String userDisplayName;
        private String userEmail;
        private String branchDisplayName;
        private String branchCode;
    }
}

