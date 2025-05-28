package com.dpw.runner.shipment.services.exception.response;

import com.dpw.runner.shipment.services.utils.Generated;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Generated
public class ShipmentDetachResponse {
    String shipmentNumber;
    Long shipmentId;
    String hbl;
}
