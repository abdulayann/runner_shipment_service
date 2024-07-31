package com.dpw.runner.shipment.services.commons.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.commons.entity.Parties;
import com.dpw.runner.shipment.services.commons.entity.enums.Ownership;
import io.swagger.annotations.ApiModel;
import lombok.*;

@Data
@Builder
@ApiModel("Truck Driver Details Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class TruckDriverDetailsRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private Long shipmentId;
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
    private Parties thirdPartyTransporter;
}
