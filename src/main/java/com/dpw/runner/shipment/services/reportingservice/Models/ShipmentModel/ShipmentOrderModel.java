package com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel;


import com.dpw.runner.shipment.services.reportingservice.Models.IDocumentModel;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class ShipmentOrderModel implements IDocumentModel {
    @JsonProperty("Id")
    private Long id;

    @JsonProperty("Guid")
    private UUID guid;

    @JsonProperty("OrderNumber")
    private String orderNumber;

    @JsonProperty("ShipmentId")
    private Long shipmentId;


}

