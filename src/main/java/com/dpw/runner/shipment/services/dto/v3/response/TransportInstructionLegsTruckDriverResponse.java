package com.dpw.runner.shipment.services.dto.v3.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import java.util.UUID;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class TransportInstructionLegsTruckDriverResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Long tiLegId;
    private String driverName;
    private String driverMobileNumber;
    private String truckNumberPlate;
    private String trailerNumberPlate;
    private String truckOrTrailerType;
}
