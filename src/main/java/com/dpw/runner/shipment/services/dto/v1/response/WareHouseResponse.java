package com.dpw.runner.shipment.services.dto.v1.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
public class WareHouseResponse {
    @JsonProperty("Id")
    private Long id;
    @JsonProperty("WarehouseDepotCodeAndDescription")
    private String warehouseDepotCodeAndDescription;
}
