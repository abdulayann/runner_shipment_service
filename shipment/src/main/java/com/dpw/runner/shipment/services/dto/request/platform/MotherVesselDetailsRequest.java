package com.dpw.runner.shipment.services.dto.request.platform;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@AllArgsConstructor
@Data
@NoArgsConstructor
public class MotherVesselDetailsRequest implements Serializable {
    private String mother_vessel_booking_number;
    private String shipping_line;
    private String vessel_name;
    private String voyage_number;
}
