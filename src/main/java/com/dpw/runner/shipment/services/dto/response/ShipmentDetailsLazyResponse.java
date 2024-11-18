package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;

import java.util.List;

@Builder
@NoArgsConstructor
@AllArgsConstructor
@Setter
@Getter
@Generated
public class ShipmentDetailsLazyResponse implements IRunnerResponse {

    private CarrierDetailResponse carrierDetails;
    private List<ContainerResponse> containersList;
    private List<BookingCarriageResponse> bookingCarriagesList;
    private List<ELDetailsResponse> elDetailsList;
    private List<EventsResponse> eventsList;
    private List<PackingResponse> packingList;
    private List<ReferenceNumbersResponse> referenceNumbersList;
    private List<RoutingsResponse> routingsList;
    private List<ServiceDetailsResponse> servicesList;
    private List<TruckDriverDetailsResponse> truckDriverDetails;
    private AdditionalDetailResponse additionalDetails;
    private List<NotesResponse> notesList;
    private PickupDeliveryDetailsResponse deliveryDetails;
    private PickupDeliveryDetailsResponse pickupDetails;
    private PartiesResponse client;
    private PartiesResponse consigner;
    private PartiesResponse consignee;
    private List<JobResponse> jobsList;
    @JsonIgnoreProperties("shipmentsList")
    private List<ConsolidationListResponse> consolidationList;
    private List<PartiesResponse> shipmentAddresses;
    private List<ShipmentOrderResponse> shipmentOrders;

}
