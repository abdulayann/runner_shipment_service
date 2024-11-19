package com.dpw.runner.shipment.services.kafka.dto;

import com.dpw.runner.shipment.services.entity.enums.ContainerPraStatus;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class CustomerContainerDto {
    private com.dpw.runner.shipment.services.kafka.dto.CustomerContainerDto.Container payload;

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class Container {
        private UUID consolidationGuid;
        private UUID containerGuid;
        private ContainerPraStatus praStatus;
    }
}
