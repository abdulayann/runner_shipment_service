package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.*;

import javax.validation.constraints.Size;
import java.util.UUID;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class TiTruckDriverDetailsRequest implements IRunnerRequest {
    private Long id;
    private UUID guid;
    private Long tiLegId;
    @Size(max=30, message = "max size is 30 for driver name")
    private String driverName;
    @Size(max=18, message = "max size is 18 for driver mobile number")
    private String driverMobileNumber;
    @Size(max=20, message = "max size is 20 for truck number plate")
    private String truckNumberPlate;
    @Size(max=20, message = "max size is 20 for trailer number plate")
    private String trailerNumberPlate;
    private String truckOrTrailerType;
}
