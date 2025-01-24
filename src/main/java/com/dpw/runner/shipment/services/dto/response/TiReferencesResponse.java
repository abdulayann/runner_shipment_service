package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.*;

import java.util.UUID;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class TiReferencesResponse implements IRunnerRequest {
    private Long id;
    private UUID guid;
    private Long tiLegId;
    private String type;
    private String reference;
}
