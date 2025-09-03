package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.enums.Ownership;
import io.swagger.annotations.ApiModel;
import lombok.*;

import javax.validation.constraints.Size;

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
    @Size(max=80, message = "max size is 80 for driver name")
    private String driverName;
    @Size(max=25, message = "max size is 25 for driver mobile number")
    private String driverMobileNumber;
    @Size(max=50, message = "max size is 50 for truck number")
    private String truckNumberPlate;
    @Size(max=50, message = "max size is 50 for trailer number")
    private String trailerNumberPlate;
    @Size(max=50, message = "max size is 50 for truck or trailer type")
    private String truckOrTrailerType;
    @Size(max=20, message = "max size is 20 for container type code")
    private String containerTypeCode;
    private Long containerId;
    private Long consolidationId;
    private String selfTransporterName;
    private String remarks;
    private String truckStatus;
    private Parties thirdPartyTransporter;
    @Size(max=50, message = "max size is 50 for driver id")
    private String driverId;
}
