package com.dpw.runner.shipment.services.dto.response.carrierbooking;

import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CarrierRoutingResponse {

    private Long id;   // from MultiTenancy / BaseEntity

    private Integer sequence;
    private String transportMode;
    private RoutingCarriage carriageType;
    private String pol;
    private String pod;
    private LocalDateTime eta;
    private LocalDateTime etd;
    private String vesselName;
    private String voyageNo;
    private Long carrierBookingId;
}
