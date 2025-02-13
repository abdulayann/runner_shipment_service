package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@Data
public class ContainerShipmentADInConsoleRequest implements IRunnerRequest {
    private ContainerRequest container;
    private List<ContainerShipmentADInConsoleRequest.PacksList> packsList;
    private Boolean isFCL;
    private Boolean isConfirmedByUser;

    @Data
    public static class PacksList implements IRunnerRequest {
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
        private PartiesRequest shipmentClientName;
        private String shipmentNumber;
        private String shipmentType;
        private Boolean hazardous;
    }
}
