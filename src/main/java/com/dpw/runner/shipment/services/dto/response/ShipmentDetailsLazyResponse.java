package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.config.CustomVolumeValueSerializer;
import com.dpw.runner.shipment.services.config.CustomWeightValueSerializer;
import com.dpw.runner.shipment.services.config.DecimalPlaceValueSerializer;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerSummaryResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.entity.enums.*;
import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Builder
@NoArgsConstructor
@AllArgsConstructor
@Data
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
}
