package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;


@Getter
@Setter
@ApiModel("Views Request Model")
@ToString
public class DefaultViewsRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private Long defaultViewId;
    private String username;
    private String entity;
}
