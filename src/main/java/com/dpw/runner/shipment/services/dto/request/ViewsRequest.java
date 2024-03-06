package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.Data;
import lombok.Getter;
import lombok.ToString;

import java.util.List;
import java.util.Map;

@Getter
@ApiModel("Views Request Model")
@ToString
@Data
public class ViewsRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private Map<String, String> columns;
    private List<FilterCriteria> criteria;
    private Boolean isPublic;
    private String entity;
    private Boolean isDefault;
}
