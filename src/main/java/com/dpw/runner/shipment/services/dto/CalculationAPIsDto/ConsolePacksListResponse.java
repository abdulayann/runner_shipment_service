package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.DedicatedMasterData;
import com.dpw.runner.shipment.services.utils.MasterData;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Data
public class ConsolePacksListResponse implements IRunnerResponse {
    private List<PacksList> packsList;
    private Boolean isFCL;

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
        private Long shipmentId;
        @DedicatedMasterData(type = Constants.COMMODITY_TYPE_MASTER_DATA)
        private String commodity;
        private String shipmentHouseBill;
        private String shipmentMasterBill;
        private PartiesResponse shipmentClient;
        private String shipmentNumber;
        private String shipmentType;
        private Map<String, String> masterData;
        private Map<String, String> commodityMasterData;
        private Boolean hazardous;
    }
}
