package com.dpw.runner.shipment.services.commons.requests;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class CommonGetRequest implements IRunnerRequest {
    private Long id;
    private String guid;
    private Boolean sectionRule;
    private List<String>includeColumns;
}
