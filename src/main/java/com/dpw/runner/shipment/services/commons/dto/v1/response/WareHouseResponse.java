package com.dpw.runner.shipment.services.commons.dto.v1.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.io.Serializable;

@Data
public class WareHouseResponse implements Serializable {
    @JsonProperty("Id")
    private Long id;
    @JsonProperty("WarehouseDepotCode")
    private String warehouseDepotCode;
    @JsonProperty("WarehouseDepotName")
    private String warehouseDepotName;
}
