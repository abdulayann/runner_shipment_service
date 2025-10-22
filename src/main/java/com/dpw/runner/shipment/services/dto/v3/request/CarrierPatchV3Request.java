package com.dpw.runner.shipment.services.dto.v3.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.openapitools.jackson.nullable.JsonNullable;

import jakarta.validation.constraints.Size;
import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CarrierPatchV3Request  extends CommonRequest implements IRunnerRequest {
    private JsonNullable<Long> id;
    private JsonNullable<String> vessel;
    @Size(max = 64, message = "max size is 64 for shipping line")
    private JsonNullable<String> shippingLine;
    @Size(max = 20, message = "max size is 20 for voyage")
    private JsonNullable<String> voyage;
    private JsonNullable<String> flightNumber;
    @Size(max = 20, message = "max size is 20 for aircraft type")
    private JsonNullable<String> aircraftType;
    @Size(max = 20, message = "max size is 20 for aircraft registration")
    private JsonNullable<String> aircraftRegistration;
    private JsonNullable<String> truckRefNumber;
    @Size(max = 10, message = "max size is 10 for journey number")
    private JsonNullable<String> journeyNumber;
    @Size(max = 10, message = "max size is 10 for journey ref number")
    private JsonNullable<String> journeyRefNumber;
    @Size(max = 100, message = "max size is 100 for origin")
    private JsonNullable<String> origin;
    @Size(max = 100, message = "max size is 100 for destination")
    private JsonNullable<String> destination;
    @ExcludeTimeZone
    private JsonNullable<LocalDateTime> etd;
    @ExcludeTimeZone
    private JsonNullable<LocalDateTime> eta;
    @ExcludeTimeZone
    private JsonNullable<LocalDateTime> ata;
    @ExcludeTimeZone
    private JsonNullable<String> originPort;
    private JsonNullable<LocalDateTime> vesselBerthingDate;
    private JsonNullable<String> destinationPort;
    private JsonNullable<Long> shipmentId;
    private JsonNullable<LocalDateTime> atd;
}