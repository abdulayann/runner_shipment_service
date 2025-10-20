package com.dpw.runner.shipment.services.dto.v3.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.*;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

import java.time.LocalDateTime;
import java.util.List;


@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@Data
public class ConsolidationDetailsV3ExternalResponse extends ConsolidationDetailsBaseResponse implements IRunnerResponse {
    private LocalDateTime latestFullEquDeliveredToCarrier;
    private LocalDateTime earliestEmptyEquPickUp;
    private LocalDateTime earliestDropOffFullEquToCarrier;
    private PartiesExternalResponse sendingAgent;
    private PartiesExternalResponse borrowedFrom;
    private PartiesExternalResponse receivingAgent;
    private List<PartiesExternalResponse> consolidationAddresses;
    private PartiesExternalResponse consigner;
    private PartiesExternalResponse consignee;
    private PartiesExternalResponse client;
    private List<RoutingsResponse> routingsList;
    private List<PackingResponse> packingList;
    private List<EventsResponse> eventsList;
    private List<ContainerResponse> containersList;
    private List<TruckDriverDetailsResponse> truckDriverDetails;
}
