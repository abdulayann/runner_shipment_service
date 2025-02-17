package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.entity.enums.ShipmentStatus;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.MasterData;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Data
public class ContainerPackSummaryDto implements IRunnerResponse {
    private List<PacksList> packsList;

    @Data
    public static class PacksList implements IRunnerResponse {
        private Long id;
        private UUID guid;
        private String packs;
        @MasterData(type = MasterDataType.PACKS_UNIT)
        private String packsType;
        private BigDecimal weight;
        @MasterData(type = MasterDataType.WEIGHT_UNIT)
        private String weightUnit;
        private BigDecimal volume;
        @MasterData(type = MasterDataType.VOLUME_UNIT)
        private String volumeUnit;
        private String goodsDescription;
        private String shipmentNumber;
        private PartiesResponse shipmentClient;
        private String shipmentHouseBill;
        private String shipmentMasterBill;
        private ShipmentStatus shipmentStatus;
        private String containerNumber;
        private Map<String, String> masterData;
    }
}
