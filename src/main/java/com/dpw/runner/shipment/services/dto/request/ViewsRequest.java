package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.Data;
import lombok.Getter;
import lombok.ToString;

import java.util.Map;

@Getter
@ApiModel("Views Request Model")
@ToString
@Data
@SuppressWarnings("java:S1948")
public class ViewsRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private Map<String, Object> columns;
    private Map<String, Object> criteria;
    private String entity;
    private String name;
    private Boolean isDefault;
}
