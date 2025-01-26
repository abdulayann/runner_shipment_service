package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.*;

import java.util.UUID;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class TiTruckDriverDetailsResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
//    private Long tiLegId;
    private String driverName;
    private String driverMobileNumber;
    private String truckNumberPlate;
    private String trailerNumberPlate;
    private String truckOrTrailerType;
}
