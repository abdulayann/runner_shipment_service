package com.dpw.runner.shipment.services.dto.request.carrierbooking;

import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CarrierRoutingRequest {

    private Integer sequence;
    private String transportMode;
    private RoutingCarriage carriageType;
    private String pol;   // Port of Loading
    private String pod;   // Port of Discharge
    @ExcludeTimeZone
    private LocalDateTime eta;
    @ExcludeTimeZone
    private LocalDateTime etd;
    private String vesselName;
    private String voyageNo;
    private Long carrierBookingId;
}
