package com.dpw.runner.shipment.services.dto.v3.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import java.util.UUID;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class TransportInstructionLegsTruckDriverRequest implements IRunnerRequest {
    private Long id;
    private UUID guid;
    @NotNull(message = "Transport Instruction leg Id is required")
    private Long tiLegId;
    @Size(max = 30, message = "max size is 30 for driver name")
    private String driverName;
    @Size(max = 18, message = "max size is 18 for driver mobile number")
    private String driverMobileNumber;
    @Size(max = 20, message = "max size is 20 for truck number plate")
    private String truckNumberPlate;
    @Size(max = 20, message = "max size is 20 for trailer number plate")
    private String trailerNumberPlate;
    private String truckOrTrailerType;
    @Size(max = 50, message = "max size is 50 for driver id")
    private String driverId;
}
