package com.dpw.runner.shipment.services.dto.patchrequest;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.openapitools.jackson.nullable.JsonNullable;

import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CarrierPatchRequest  extends CommonRequest implements IRunnerRequest {
    private JsonNullable<Long> id;
    private JsonNullable<String> shippingLine;
    private JsonNullable<String> vessel;
    private JsonNullable<String> voyage;
    private JsonNullable<String> flightNumber;
    private JsonNullable<String> aircraftType;
    private JsonNullable<String> aircraftRegistration;
    private JsonNullable<String> truckRefNumber;
    private JsonNullable<String> journeyNumber;
    private JsonNullable<String> journeyRefNumber;
    private JsonNullable<String> origin;
    private JsonNullable<String> destination;
    @ExcludeTimeZone
    private JsonNullable<LocalDateTime> eta;
    @ExcludeTimeZone
    private JsonNullable<LocalDateTime> etd;
    @ExcludeTimeZone
    private JsonNullable<LocalDateTime> ata;
    @ExcludeTimeZone
    private JsonNullable<LocalDateTime> atd;
    private JsonNullable<Long> shipmentId;
    private JsonNullable<String> originPort;
    private JsonNullable<String> destinationPort;
    private JsonNullable<LocalDateTime> vesselBerthingDate;
}
