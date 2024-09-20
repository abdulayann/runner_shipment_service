package com.dpw.runner.shipment.services.dto.request.crp;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.*;

@Getter
@Setter
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class CRPRetrieveRequest implements IRunnerRequest {
    private String searchString;
}
