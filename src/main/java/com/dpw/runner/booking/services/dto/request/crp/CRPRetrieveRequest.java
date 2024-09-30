package com.dpw.runner.booking.services.dto.request.crp;

import com.dpw.runner.booking.services.commons.requests.IRunnerRequest;
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
