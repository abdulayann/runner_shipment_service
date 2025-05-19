package com.dpw.runner.shipment.services.dto.shipment_console_dtos;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
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
public class ShipmentPacksAssignContainerTrayDto implements IRunnerResponse {

    private List<Shipments> shipmentsList;
    private Boolean isFCLShipmentAssigned;
    private Long assignedFCLShipment;

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
        private BigDecimal weight;
        private String weightUnit;
        private BigDecimal netWeight;
        private String netWeightUnit;
        private BigDecimal volume;
        private String volumeUnit;
        private List<Packages> packsList;
        private boolean selectedContainerAssigned;
        private String shipmentType;
        private Long containerAssignedToShipmentCargo;

        @Data
        @Builder
        @AllArgsConstructor
        @NoArgsConstructor
        public static class Packages {
            private Long id;
            private UUID guid;
            private String commodity;
            private BigDecimal weight;
            private String weightUnit;
            private BigDecimal netWeight;
            private String netWeightUnit;
            private String packs;
            private String packsType;
            private BigDecimal volume;
            private String volumeUnit;
            private Long containerId;
            private String containerNumber;
            private String containerCode;
        }
    }
}
