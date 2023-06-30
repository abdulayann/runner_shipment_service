package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.entity.Parties;
import io.swagger.annotations.ApiModel;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.time.LocalDateTime;
import java.util.List;

@Getter
@Setter
@ApiModel("Pickup Delivery Details Request Model")
@ToString
public class PickupDeliveryDetailsRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private LocalDateTime estimatedPickupOrDelivery;
    private LocalDateTime requiredBy;
    private LocalDateTime portTransportAdvised;
    private LocalDateTime actualPickupOrDelivery;
    private LocalDateTime pickupOrDelivery;
    private List<Parties> partiesBrokerDetailsList;
    private List<Parties> partiesAgentDetailsList;
    private List<Parties> partiesTransporterDetailsList;
    private List<Parties> partiesDestinationDetailsList;
    private String type;
    private Long shipmentId;
}
