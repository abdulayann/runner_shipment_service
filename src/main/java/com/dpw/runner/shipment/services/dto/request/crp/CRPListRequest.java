package com.dpw.runner.shipment.services.dto.request.crp;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.*;

import java.util.List;

@Getter
@Setter
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class CRPListRequest  implements IRunnerRequest {
    private String searchString;
    private boolean isBillable;
    private Pageable pageable;

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    private static class Pageable {
        private int page;
        private int size;
        private List<String> sort;
    }
}
