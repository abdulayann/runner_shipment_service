package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@Data
public class ConsolePacksListResponse implements IRunnerResponse {
    private List<PacksList> packsList;
    private Boolean isFCL;

    @Data
    public static class PacksList {
        private Long id;
        private UUID guid;
        private String packs;
        private String packsType;
        private BigDecimal weight;
        private String weightUnit;
        private BigDecimal volume;
        private String volumeUnit;
        private Long shipmentId;
        private String commodity;
        private String shipmentHouseBill;
        private String shipmentMasterBill;
        private String shipmentClientName;
        private String shipmentNumber;
        private String shipmentType;
    }
}
