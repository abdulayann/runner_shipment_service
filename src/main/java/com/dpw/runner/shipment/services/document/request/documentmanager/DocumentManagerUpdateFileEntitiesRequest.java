package com.dpw.runner.shipment.services.document.request.documentmanager;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class DocumentManagerUpdateFileEntitiesRequest {
    private List<UpdateFileRequest> filesToUpdate;

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class UpdateFileRequest {
        private EntityData source;
        private List<EntityData> entitiesToAttach;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class EntityData {
        private String entityType;
        private String entityKey;
    }
}

