package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.util.List;

@Data
@ApiModel("Complete Shipment Request Model")
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CompleteShipmentRequest extends CommonRequest implements IRunnerRequest {
    private List<PackingRequest> packingRequest;
    private List<AdditionalDetailRequest> additionalDetailRequest;
    private List<BookingCarriageRequest> bookingCarriageRequest;
    private List<ContainerRequest> containerRequest;
    private List<ELDetailsRequest> elDetailsRequest;
    private List<EventsRequest> eventsRequest;
    private List<FileRepoRequest> fileRepoRequest;
    private List<JobRequest> jobRequest;
    private List<NotesRequest> notesRequest;
    private List<ReferenceNumbersRequest> referenceNumbersRequest;
    private List<RoutingsRequest> routingsRequest;
    private List<ServiceDetailsRequest> serviceDetailsRequest;
    private List<CarrierDetailRequest> carrierDetailRequest;
    private List<PickupDeliveryDetailsRequest> pickupDeliveryDetailsRequest;
    private List<PartiesRequest> partiesRequest;
    private ShipmentRequest shipmentRequest;
}
