package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.*;
import lombok.Getter;

import java.util.List;

@Getter
public class CompleteShipmentResponse implements IRunnerResponse {
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

