package com.dpw.runner.shipment.services.kafka.dto;

import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data @Builder
@AllArgsConstructor @NoArgsConstructor
public class PushToDownstreamEventDto {
    private Long parentEntityId;
    private String parentEntityName;
    private String action;
    private Meta meta = new Meta();
    List<Triggers> triggers;



    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class Triggers {
        private Long entityId;
        private String entityName;
        private String action;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class Meta {
        private Boolean isCreate = false;
        private Boolean isAutoSellRequired = false;
        private Integer tenantId;
        private String sourceInfo;
    }
}

