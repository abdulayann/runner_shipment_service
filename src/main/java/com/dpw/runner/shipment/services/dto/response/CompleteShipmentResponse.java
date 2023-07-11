package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.util.List;

@Data
@Builder
@ApiModel("Complete Shipment Response Model")
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class CompleteShipmentResponse implements IRunnerResponse {
    private ShipmentDetailsResponse shipment;
    private List<PackingResponse> packings;
    private List<AdditionalDetailResponse> additionalDetails;
    private List<BookingCarriageResponse> bookingCarriages;
    private List<ContainerResponse> containers;
    private List<ELDetailsResponse> elDetails;
    private List<EventsResponse> events;
    private List<FileRepoResponse> fileRepo;
    private List<JobResponse> job;
    private List<NotesResponse> notes;
    private List<ReferenceNumbersResponse> referenceNumbers;
    private List<RoutingsResponse> routings;
    private List<ServiceDetailsResponse> serviceDetails;
    private List<CarrierDetailResponse> carrierDetails;
    private List<PickupDeliveryDetailsResponse> pickupDeliveryDetails;
    private List<PartiesResponse> parties;

}
