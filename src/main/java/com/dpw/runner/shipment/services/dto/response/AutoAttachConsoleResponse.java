package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.*;

import java.util.UUID;
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
public class AutoAttachConsoleResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private String consolidationNumber;
    private String transportMode;
    private CarrierDetailResponse carrierDetails;
    private PartiesResponse client;
    private PartiesResponse consigner;
    private PartiesResponse consignee;
}
