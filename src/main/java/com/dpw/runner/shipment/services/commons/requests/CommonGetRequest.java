package com.dpw.runner.shipment.services.commons.requests;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class CommonGetRequest implements IRunnerRequest {
    private Long id;
    private String guid;
    private Boolean sectionRule;
    private List<String>includeColumns;
}
