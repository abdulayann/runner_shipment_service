package com.dpw.runner.shipment.services.dto.request.platform;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class VehicleDetailsDTO {
    private String containerGuid;
    private String trailerNumber;

}

