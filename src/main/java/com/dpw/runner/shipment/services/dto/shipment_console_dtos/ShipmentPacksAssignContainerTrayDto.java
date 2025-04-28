package com.dpw.runner.shipment.services.dto.shipment_console_dtos;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ShipmentPacksAssignContainerTrayDto {

    private List<Shipments> shipmentsList;
    private Boolean isFCLShipmentAssigned;

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class Shipments {
        private Long id;
        private UUID guid;
        private String shipmentId;
        private String houseBill;
        private Integer noOfPacks;
        private String packsUnit;
        private BigDecimal netWeight;
        private String netWeightUnit;
        private BigDecimal volume;
        private String volumeUnit;
        private List<Packages> packingList;
        private boolean assigned;
        private String shipmentType;

        @Data
        @Builder
        @AllArgsConstructor
        @NoArgsConstructor
        private static class Packages {
            private String commodity;
            private BigDecimal netWeight;
            private String netWeightUnit;
            private String packs;
            private String packsType;
            private BigDecimal volume;
            private String volumeUnit;
            private Long containerId;
        }
    }
}
