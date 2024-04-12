package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.enums.Ownership;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.util.UUID;

@Data
@Builder
@ApiModel("Truck Driver Details Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class TruckDriverDetailsResponse implements IRunnerResponse {
    private Long id;
    private Long shipmentId;
    private UUID guid;
    private Ownership transporterType;
    private String transporterName;
    private String driverName;
    private String driverMobileNumber;
    private String truckNumberPlate;
    private String trailerNumberPlate;
    private String truckOrTrailerType;
    private String containerTypeCode;
    private Long containerId;
    private Long consolidationId;
    private String selfTransporterName;
    private String remarks;
    private String truckStatus;
    private String shipmentNumber;
    private Parties thirdPartyTransporter;
}
