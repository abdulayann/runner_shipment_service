package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Data;

import java.util.List;
import java.util.Map;
import java.util.UUID;

@Data
public class ViewsResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Map<String, Object> columns;
    private Map<String, Object> criteria;
    private String entity;
    private String name;
    private String createdBy;
    private Boolean isDefault;
}
