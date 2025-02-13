package com.dpw.runner.shipment.services.dto.request.crp;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.io.Serializable;
import java.util.List;

@Getter
@Setter
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class CRPListRequest implements IRunnerRequest {
    private String searchString;
    @JsonProperty("isBillable")
    private boolean isBillable;
    private Pageable pageable;

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    private static class Pageable implements Serializable {
        private int page;
        private int size;
        private List<String> sort;
    }
}
