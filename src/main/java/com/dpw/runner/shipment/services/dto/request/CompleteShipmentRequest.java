package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Getter;

import java.util.List;

@Getter
public class CompleteShipmentRequest implements IRunnerRequest {
    private List<AdditionalDetailRequest> additionalDetailRequest;
    private List<BookingCarriageRequest>  bookingCarriageRequest;
    private List<ContainerRequest> containerRequest;
    private List<ELDetailsRequest> elDetailsRequest;
    private List<EventsRequest> eventsRequest;
    private List<FileRepoRequest> fileRepoRequest;
    private List<JobRequest> jobRequest;
    private List<NotesRequest> notesRequest;
    private List<ReferenceNumbersRequest> referenceNumbersRequest;
    private List<RoutingsRequest> routingsRequest;
    private List<ServiceDetailsRequest> serviceDetailsRequest;
    private ShipmentRequest shipmentRequest;
}

