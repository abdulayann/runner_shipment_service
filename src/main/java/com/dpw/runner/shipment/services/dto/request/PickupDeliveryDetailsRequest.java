package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.entity.PartiesDetails;
import io.swagger.annotations.ApiModel;
import lombok.Getter;
import lombok.ToString;

import java.time.LocalDateTime;
import java.util.List;

@Getter
@ApiModel("Pickup Delivery Details Request Model")
@ToString
public class PickupDeliveryDetailsRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private LocalDateTime estimatedPickupOrDelivery;
    private LocalDateTime requiredBy;
    private LocalDateTime portTransportAdvised;
    private LocalDateTime actualPickupOrDelivery;
    private LocalDateTime pickupOrDelivery;
    private List<PartiesDetails> partiesBrokerDetailsList;
    private List<PartiesDetails> partiesAgentDetailsList;
    private List<PartiesDetails> partiesTransporterDetailsList;
    private List<PartiesDetails> partiesDestinationDetailsList;
    private String type;
    private Long shipmentId;
}
