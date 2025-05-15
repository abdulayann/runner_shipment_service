package com.dpw.runner.shipment.services.kafka.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data @Builder
@AllArgsConstructor @NoArgsConstructor
public class PushToDownstreamEventDto {
    private Long parentEntityId;
    private String parentEntityName;
    private String action;
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
}

