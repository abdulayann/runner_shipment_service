package com.dpw.runner.shipment.services.commons.dto.request.platform;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class VehicleDetailsDTO implements Serializable {
    private String containerGuid;
    private String trailerNumber;

}

